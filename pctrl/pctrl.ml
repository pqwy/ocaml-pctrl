open Lwt

let strf = Fmt.strf
let failwiths fmt = Fmt.kstrf failwith fmt

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

module POBox = struct

  type ('a, 'b) t = ('a, 'b Lwt.u) Hashtbl.t

  let create () = Hashtbl.create 16

  let waitfor t k = let (th, u) = wait () in Hashtbl.add t k u; th

  let continue t k v =
    let u = Hashtbl.find t k in
    Hashtbl.remove t k; wakeup u v

  let is_waiting t k = Hashtbl.mem t k
end

(** context **)

let shim = Pctrl_prefix_gen.prefix ^ "/lib/stublibs/dllinstrumentation_stubs.so"

type ctx = {
  efd        : Unix.file_descr
; sigin      : int
; sigout     : int
; post       : (int, int) POBox.t
; timeout    : float
; env        : (string * string) list
; mutable id : int
}

module Siginfo_t = Signal_fd.Siginfo_t

let rec receive fd buf p =
  Lwt_unix.read fd buf 0 (Bytes.length buf) >>= fun _ ->
    let (pid, cookie) = Siginfo_t.(get_pid buf, get_int buf) in
    if POBox.is_waiting p cookie then
      POBox.continue p cookie pid
    else Fmt.pr "* Ack %d was not expected. Something's racy...\n%!" cookie ;
    receive fd buf p

let create ?(timeout=5.)
           ?(env=[])
           ?(sigout=Signal_fd.sigrtmin)
           ?(sigin=Signal_fd.sigrtmin + 1) () =
  Fs.required shim;
  Signal_fd.sigprocmask `Block [sigin] |> ignore ;
  let efd  = Signal_fd.signalfd ~cloexec:true [sigin]
  and post = POBox.create () in
  async (fun () ->
    receive (Lwt_unix.of_unix_file_descr efd)
            (Bytes.create Siginfo_t.size) post) ;
  { efd; sigin; sigout; post; timeout; env; id = 1 }

let destroy ctx =
  Signal_fd.sigprocmask `Unblock [ctx.sigin] |> ignore ;
  Unix.close ctx.efd


(** process **)

type p = { pid : int ; ctx : ctx ; mutex : Lwt_mutex.t }

type stat = string * char

exception Dead of int * string * string

let serially p = Lwt_mutex.with_lock p.mutex

let pid { pid } = pid

let of_pid ~ctx pid = { pid ; ctx ; mutex = Lwt_mutex.create () }

let p_stat pid =
  let parse s =
    Scanf.sscanf s "%d %s %c" (fun _ comm state -> (comm, state))
  and fp  = strf "/proc/%d/stat" pid
  and buf = Bytes.create 256 in
  let open Lwt_unix in try%lwt
    with_open_file fp [O_RDONLY] 0 @@ fun fd ->
      read fd buf 0 256 >|= fun _ -> Some (parse buf)
  with Unix.Unix_error (Unix.ENOENT, _, _) -> return_none

let pp_stat =
  Fmt.(option ~none:(const string "<GONE>") (Dump.pair string char))

let itsdead { pid } fmt =
  let k msg = p_stat pid >>= fun s ->
    fail (Dead (pid, msg, Fmt.strf "%a" pp_stat s)) in
  Fmt.kstrf k fmt

let environment ?(extra=[]) instrument =
  let a1 =
    if instrument then [| strf "LD_PRELOAD=%s" shim |] else [| |]
  and a2 =
    extra |> List.map (fun (k, v) -> strf "%s=%s" k v)
          |> Array.of_list in
  Array.(append (append a1 a2) (Unix.environment ()))

let start ?(instrument=true) ctx exe args =
  Fs.required exe;
  let env  = environment ~extra:ctx.env instrument
  and args = Array.of_list (exe::args) in
  let open Unix in match fork () with
  | 0   -> Unix.execve exe args env
  | pid -> Lwt_unix.sleep 0.1 >|= fun () -> of_pid ~ctx pid
  | exception Unix_error (EAGAIN, _, _) ->
      failwiths "Pctrl.start: failed to fork: EAGAIN"

let new_cookie ({ id } as ctx) = ctx.id <- id + 1; id

let timeout ~time f =
  (f >|= (fun x -> `Ok x)) <?> (Lwt_unix.sleep time >|= fun () -> `Timeout)

let clone ({ pid; ctx } as p) =
  serially p @@ fun () ->
    let cookie = new_cookie ctx in
    let th = POBox.waitfor ctx.post cookie >|= of_pid ~ctx in
    let rec wkup = function
      | 0 -> itsdead p "Clone timeout (cookie %d)" cookie
      | n ->
          Unix.kill p.pid Sys.sigcont;
          timeout ~time:0.01 th >>= function
            | `Ok x    -> return x
            | `Timeout -> wkup (pred n) in
    try%lwt
      Signal_fd.sigqueue ~pid ~sgn:ctx.sigout ~value:cookie;
      wkup (int_of_float (ctx.timeout /. 0.01))
    with Unix.Unix_error (Unix.ESRCH, _, _) -> itsdead p "Clone ESRCH"

let terminate p =
  let send () =
    let open Unix in try
      kill p.pid Sys.sigterm;
      kill p.pid Sys.sigcont
    with Unix_error (ESRCH, _, _) -> () in
  serially p @@ fun () -> send () |> return

let signal ({ pid; ctx } as p) sign cond =
  let kill () = Unix.(
    try kill pid sign |> return with
      Unix_error (ESRCH, _, _) -> itsdead p "Trying to send signal."
  ) in
  let rec go = function
    | 0 -> itsdead p "Waiting for signal to cause state transition"
    | n ->
        kill () >>= fun () -> p_stat pid >>= function
          | None -> itsdead p "Trying to send signal."
          | Some (_, s) when cond s -> return_unit
          | Some _ -> Lwt_unix.sleep 0.01 >>= fun () -> go (pred n) in
  serially p @@ fun () -> go (int_of_float (ctx.timeout /. 0.01))

let freeze p = signal p Sys.sigstop (fun s -> s = 'T')
let unfreeze p = signal p Sys.sigcont (fun s -> s <> 'T')
