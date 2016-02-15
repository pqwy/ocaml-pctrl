open Lwt

let failwiths fmt = Fmt.kstrf failwith fmt

let lwrite fd buf =
  let rec go i n =
    Lwt_unix.write fd buf i n >>= function
      | k when k < n -> go (i + k) (n - k)
      | _            -> Lwt.return_unit in
  go 0 (Bytes.length buf)

let chunks ~sep s =
  let open String in
  let n = length s in
  let rec go i =
    match index_from s i sep with
    | exception Not_found -> [sub s i (n - i)]
    | k -> sub s i (k - i) :: go (k + 1) in
  go 0

module Fd = struct
  let of_int n =
    let fd = Obj.magic n in
    match Unix.read fd "" 0 0 with
    | 0           -> Some fd
    | _           -> None
    | exception _ -> None
  let to_int = Obj.magic
end
