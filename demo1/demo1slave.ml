open Common

let envvar = "TRACE_FD"

let reread =
  let open Unix in
  let rec go buf fd =
    match read fd buf 0 (Bytes.length buf) with
    | k -> Bytes.sub buf 0 k
    | exception Unix_error (EINTR, _, _) -> go buf fd in
  go (Bytes.create 1024)

let prf fmt = Fmt.pr ("[slave %d] " ^^ fmt ^^ "\n%!") (Unix.getpid ())

let main () =
  let fd =
    match Unix.getenv envvar |> int_of_string |> Fd.of_int with
    | exception Not_found -> failwiths "%s not in environment." envvar
    | exception Failure _ -> failwiths "%s is not parsable." envvar
    | None                -> failwiths "fd from %s not usable?" envvar
    | Some fd             -> fd in
  prf "acquired the fd.";
  let rec accum overflow acc = match overflow with
    | [] | [_] ->
        let frag = match overflow with x::_ -> x | _ -> "" in
        let res  = reread fd in
        accum ((frag ^ res) |> chunks ~sep:'\n') acc
    | "END"::_ -> prf "stop. acc: %d" acc
    | msg::msgs ->
        match int_of_string msg with
        | exception Failure _ -> prf "junk: %s" msg; accum msgs acc
        | x                   -> prf "recv: %d" x; accum msgs (acc + x)
  in
  accum [] 0

let () = main ()
