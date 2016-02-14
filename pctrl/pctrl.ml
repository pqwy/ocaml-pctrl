open Lwt

let strf = Fmt.strf
let failwiths fmt = Fmt.kstrf failwith fmt

module Fs = struct

  let exists path =
    let open Unix in
    try (stat path |> ignore; true) with Unix_error (ENOENT, _, _) -> false

  let required path =
    if not (exists path) then failwiths "File not found: " path
end

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

let helper =
  Pctrl_prefix_gen.prefix
    ^ "/lib/stublibs/dllinstrumentation_stubs.so"

let () = Fs.required helper

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

type t = { pid : int ; mutex : Lwt_mutex.t }

let pid { pid } = pid

let of_pid pid = { pid ; mutex = Lwt_mutex.create () }

let postmortem pid =
  let open Lwt_unix in
  let rec go buf =
    try%lwt
      openfile (strf "/proc/%d/stat" pid) [O_RDONLY] 0 >>= fun fd ->
        read fd buf 0 1024 >|= Bytes.sub buf 0
    with Unix.Unix_error (Unix.ENOENT, _, _) -> return "<GONE>"
  in go (Bytes.create 1024)

exception Dead of int * string * string

let itsdead pid fmt =
  postmortem pid >>= fun status ->
    Printf.ksprintf (fun msg -> fail (Dead (pid, msg, status))) fmt

let environment ?(extra=[]) instrument =
  let a1 =
    if instrument then [| strf "LD_PRELOAD=%s" helper |] else [| |]
  and a2 =
    extra |> List.map (fun (k, v) -> strf "%s=%s" k v)
          |> Array.of_list in
  Array.(append (append a1 a2) (Unix.environment ()))

let start ?(instrument=true) ctx exe args =
  Fs.required exe;
  let env  = environment ~extra:ctx.env instrument
  and args = Array.of_list (exe::args) in
  match Unix.fork () with
  | 0   -> Unix.execve exe args env
  | pid -> Lwt_unix.sleep 0.1 >|= fun () -> of_pid pid

let new_cookie ({ id } as ctx) = ctx.id <- id + 1; id

let timeout ~time f =
  (f >|= (fun x -> `Ok x)) <?> (Lwt_unix.sleep time >|= fun () -> `Timeout)

let clone ctx ({ pid } as t) =
  Lwt_mutex.with_lock t.mutex @@ fun () ->
    let cookie = new_cookie ctx in
    let th = POBox.waitfor ctx.post cookie >|= of_pid in
    let rec wkup = function
      (* | 0 -> itsdead t.pid "Clone timeout (cookie %d)" cookie *)
      | 0 -> itsdead t.pid "Clone timeout."
      | n ->
          Unix.kill t.pid Sys.sigcont;
          timeout ~time:0.5 th >>= function
            | `Ok x    -> return x
            | `Timeout -> wkup (pred n) in
    try%lwt
      Signal_fd.sigqueue ~pid ~sgn:ctx.sigout ~value:cookie;
      wkup (int_of_float (ctx.timeout /. 0.5))
    with Unix.Unix_error (Unix.ESRCH, _, _) -> itsdead t.pid "Clone ESRCH"

let terminate t =
  let open Unix in
  try
    kill t.pid Sys.sigterm;
    kill t.pid Sys.sigcont
  with Unix_error (ESRCH, _, _) -> ()

(* XXX *)
let signal ({ pid } as t) sign =
  Lwt_mutex.with_lock t.mutex @@ fun () ->
    Unix.kill pid sign; Lwt_unix.sleep 0.01

let freeze t = signal t Sys.sigstop
let unfreeze t = signal t Sys.sigcont
