open Lwt

let env_data = "NQSB_DATA"
let env_ctrl = "NQSB_CONTROL"

let failwiths fmt = Fmt.kstrf failwith fmt

let result th =
  try%lwt th >|= fun x -> `Ok x with err -> return (`Error err)

let die fmt =
  Fmt.kstrf (fun msg -> Fmt.pr "[ERROR] %s\n%!" msg; exit 1) fmt

module Fd : sig
  val of_int : int -> Unix.file_descr option
  val to_int : Unix.file_descr -> int
  val of_string : string -> Unix.file_descr option
  val to_string : Unix.file_descr -> string
end = struct
  let of_int n =
    let fd = Obj.magic n in
    match Unix.read fd "" 0 0 with
    | 0           -> Some fd
    | _           -> None
    | exception _ -> None
  let to_int = Obj.magic
  let of_string s =
    try int_of_string s |> of_int with Invalid_argument _ -> None
  let to_string i = to_int i |> string_of_int
end

module Fs = struct

  let exists path =
    let open Unix in
    try (stat path |> ignore; true) with Unix_error (ENOENT, _, _) -> false

  let required path =
    if not (exists path) then failwiths "File not found: %s" path
end

let with_open_file path flags perm k =
  let open Lwt_unix in
  openfile path flags perm >>= fun fd ->
    k fd >>= fun res -> close fd >|= fun () -> res

module POBox : sig
  type ('a, 'b) t
  val create : unit -> ('a, 'b) t
  val waitfor : ('a, 'b) t -> 'a -> 'b Lwt.t
  val continue : ('a, 'b) t -> 'a -> 'b -> unit
  val is_waiting : ('a, 'b) t -> 'a -> bool
end = struct

  type ('a, 'b) t = ('a, 'b Lwt.u) Hashtbl.t

  let create () = Hashtbl.create 16

  let waitfor t k = let (th, u) = wait () in Hashtbl.add t k u; th

  let continue t k v =
    let u = Hashtbl.find t k in
    Hashtbl.remove t k; wakeup u v

  let is_waiting t k = Hashtbl.mem t k
end

(** context **)

let shim  = Pctrl_prefix_gen.prefix ^ "/lib/stublibs/dllinstrumentation_stubs.so"
let signo = Signal_fd.sigrtmin

type ctx = {
    control    : (Unix.file_descr * Unix.file_descr)
  ; pobox      : (int, (int * Lwt_unix.file_descr)) POBox.t
  ; timeout    : float
  ; env        : (string * string) list
  ; mutable id : int
  }

let bufsize = 1024

let rec receive p fd buf =
  let recv () =
    Sendmsg_lwt.recv fd buf 0 bufsize >|= fun (n, x) ->
      (Bytes.(sub buf 0 n |> unsafe_to_string), x) in
  recv () >>= function
    | (msg, None)   -> die "Control channel: no socket: %S" msg
    | (msg, Some x) ->
        match Scanf.sscanf msg "%d %d\n" (fun a b -> (a, b)) with
        | (cookie, pid) -> POBox.continue p cookie (pid, x); receive p fd buf
        | exception Scanf.Scan_failure err ->
            die "Control channel: rubbish data: %s" msg

let create ?(timeout=5.) ?(env=[]) () =
  Fs.required shim;
  let (c1, c2) = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let pobox = POBox.create () in
  let env   = (env_ctrl, Fd.to_string c2) :: env in
  async (fun () ->
    receive pobox (Lwt_unix.of_unix_file_descr c1) (Bytes.create bufsize)
  );
  Unix.set_close_on_exec c1;
  { control = (c1, c2); pobox; timeout; env; id = 1 }

let destroy ctx =
  let (c1, c2) = ctx.control in
  Unix.(close c1; close c2)

let retry ?(precision=0.01) ctx ~f ~errk =
  let rec go = function
    | 0 -> errk ()
    | n ->
        Lwt_unix.sleep precision >>= fun () -> f () >>= function
          | Some res -> return res | None -> go (pred n) in
  let steps = int_of_float (ctx.timeout /. precision) in
  go steps

(** stat **)

type stat = string * char

let stat pid =
  let parse s =
    Scanf.sscanf s "%d %s %c" (fun _ comm state -> (comm, state))
  and fp  = Fmt.strf "/proc/%d/stat" pid
  and buf = Bytes.create 256 in
  let open Lwt_unix in try%lwt
    with_open_file fp [O_RDONLY] 0 @@ fun fd ->
      read fd buf 0 256 >|= fun _ -> Some (parse buf)
  with Unix.Unix_error (Unix.ENOENT, _, _) -> return_none

let pp_stat =
  Fmt.(option ~none:(const string "<GONE>") (Dump.pair string char))

exception Dead of int * string * string

let itsdead_pid ?(hook=fun () -> return_unit) pid fmt =
  let k msg =
    let t1 = stat pid
    and t2 = hook () in
    t1 >>= fun s -> t2 >>= fun () ->
      fail (Dead (pid, msg, Fmt.strf "%a" pp_stat s)) in
   Fmt.kstrf k fmt

let wait_stat ?precision ~ctx pid f =
  retry ?precision ctx
    ~f:(fun () -> stat pid >>= f)
    ~errk:(fun () -> itsdead_pid pid "Waiting for state transition")

let sleeping_pid ?precision ~ctx pid =
  wait_stat ?precision ~ctx pid @@ function
    | Some (_, 'S') -> return_some ()
    | None          -> itsdead_pid pid "Waiting for sleep"
    | _             -> return_none


(** process **)

type p = { pid : int ; ctx : ctx ; fd : Lwt_unix.file_descr ; mx : Lwt_mutex.t }

let create_p ~ctx ~pid ~fd = { ctx; pid; fd; mx = Lwt_mutex.create () }

let itsdead { pid; fd } fmt =
  itsdead_pid ~hook:(fun () -> Lwt_unix.close fd) pid fmt

let wait_sleeping ?precision p = sleeping_pid ?precision ~ctx:p.ctx p.pid

let pid { pid; _ } = pid

let fd { fd; _ } = fd

let serially t f = Lwt_mutex.with_lock t.mx f

let environment instrument env =
  let (++) = Array.append in
  (if instrument then [| Fmt.strf "LD_PRELOAD=%s" shim |] else [| |])
  ++
  (env |> List.map (fun (k, v) -> Fmt.strf "%s=%s" k v) |> Array.of_list)
  ++
  Unix.environment ()

let start ?(instrument=true) ctx exe args =
  Fs.required exe;
  let (s1, s2) = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let env  = environment instrument ((env_data, Fd.to_string s2)::ctx.env)
  and args = Array.of_list (exe::args) in
  let open Unix in match fork () with
  | 0   -> Unix.(close s1; execve exe args env)
  | pid ->
      Unix.close s2;
      sleeping_pid ~ctx pid >|= fun () ->
        create_p ~ctx ~fd:(Lwt_unix.of_unix_file_descr s1) ~pid
  | exception Unix_error (EAGAIN, _, _) ->
      failwiths "Pctrl.start: failed to fork: EAGAIN"

let new_cookie ({ id } as ctx) = ctx.id <- id + 1; id

let timeout ~time f =
  (f >|= (fun x -> `Ok x)) <?> (Lwt_unix.sleep time >|= fun () -> `Timeout)

let clone ({ pid; ctx } as p) =
  let cookie = new_cookie ctx in
  let t1 =
    try
      Signal_fd.sigqueue ~pid ~sgn:signo ~value:cookie |> return
    with Unix.Unix_error (Unix.ESRCH, _, _) -> itsdead p "Clone ESRCH" in
  t1 >>= fun () ->
    timeout ~time:ctx.timeout
      (POBox.waitfor ctx.pobox cookie >|= fun (pid, fd) -> create_p ~ctx ~pid ~fd)
  >>= function
  | `Timeout -> itsdead p "Clone timeout (cookie: %d)" cookie
  | `Ok p    -> return p

let terminate p =
  let send () =
    let open Unix in try
      kill p.pid Sys.sigterm;
      kill p.pid Sys.sigcont
    with Unix_error (ESRCH, _, _) -> () in
  serially p @@ fun () -> Lwt_unix.close (fd p) >|= send

let signal ({ pid; ctx } as p) ~sign cond =
  let kill () = Unix.(
    try kill pid sign |> return with
      Unix_error (ESRCH, _, _) -> itsdead p "Trying to send signal."
  ) in
  let rec go = function
    | 0 -> itsdead p "Waiting for signal to cause state transition"
    | n ->
        kill () >>= fun () -> stat pid >>= function
          | None -> itsdead p "Trying to send signal."
          | Some (_, s) when cond s -> return_unit
          | Some _ -> Lwt_unix.sleep 0.01 >>= fun () -> go (pred n) in
  serially p @@ fun () -> go (int_of_float (ctx.timeout /. 0.01))

let freeze p = signal p Sys.sigstop (fun s -> s = 'T')
let unfreeze p = signal p Sys.sigcont (fun s -> s <> 'T')

let wait_dead ?precision { ctx; pid } =
  wait_stat ?precision ~ctx pid @@ function
    | None | Some (_, 'Z') -> return_some ()
    | _ -> return_none
