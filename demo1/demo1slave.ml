open Common

let envvar = "NQSB_DATA"

let rec eintr f a =
  try f a with Unix.Unix_error (Unix.EINTR, _, _) -> eintr f a

let read =
  let buf = Bytes.create 1024 in fun fd ->
    eintr Unix.(read fd buf 0) 1024 |> Bytes.sub buf 0

let write fd buf =
  let rec go i = function
    | 0 -> ()
    | n -> let k = eintr Unix.(write fd buf 0) n in go (i + k) (n - k) in
  go 0 (Bytes.length buf)

(* let reread = *)
(*   let open Unix in *)
(*   let rec go buf fd = *)
(*     match read fd buf 0 (Bytes.length buf) with *)
(*     | k -> Bytes.sub buf 0 k *)
(*     | exception Unix_error (EINTR, _, _) -> go buf fd in *)
(*   go (Bytes.create 1024) *)

(* let write fd buf = *)
(*   let open Unix in *)
(*   let rec go i = function *)
(*     | 0 -> () *)
(*     | n -> match write fd buf i n with *)
(*       | k -> go (i + k) (n - k) *)
(*       | exception Unix_error (EINTR, _, _) -> go i n in *)
(*   go 0 (Bytes.length buf) *)

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
        ( match read fd with
          | ""  -> prf "stop (eof). acc: %d" acc
          | res -> accum ((frag ^ res) |> chunks ~sep:'\n') acc )
        (* let res  = read fd in *)
        (* accum ((frag ^ res) |> chunks ~sep:'\n') acc *)
    | "END"::_ -> prf "stop. acc: %d" acc
    | msg::msgs ->
        match int_of_string msg with
        | exception Failure _ -> prf "junk: %s" msg; accum msgs acc
        | x                   ->
            prf "recv: %d" x;
            write fd ".";
            prf "acked.";
            accum msgs (acc + x)
  in
  accum [] 0

let () = main ()
