open Lwt.Infix
open Common

let envvar = "TRACE_FD"

let rec (--) a b = if a > b then [] else a :: succ a -- b

let send_range fd a b =
  (a -- b) |> Lwt_list.map_s @@ fun n ->
    lwrite fd (string_of_int n ^ "\n")

let lswrite fd x = lwrite fd x >>= fun () -> Lwt_unix.sleep 0.1

let lswrite_i fd x = lswrite fd (string_of_int x ^ "\n")

let amb p xs =
  xs |> Lwt_list.map_s @@ fun x ->
    Pctrl.clone p >|= fun q -> (q, x)

let ambk p xs k =
  xs |> Lwt_list.iter_s @@ fun x ->
    Pctrl.clone p >>= fun q ->
      Pctrl.unfreeze q >>= fun () ->
        k q x

module K = struct

  type 'a t =
      Pure   : 'a -> 'a t
    | Impure : 'x list * ('x -> 'a t) -> 'a t

  let return x = Pure x

  module Infix = struct
    let rec (>>=) : type a b. a t -> (a -> b t) -> b t =
      fun a fb -> match a with
      | Pure x          -> fb x
      | Impure (amb, k) -> Impure (amb, fun x -> k x >>= fb)
    let (>|=) a f = a >>= fun x -> return (f x)
  end

  let amb xs = Impure (xs, return)
end

let explore1 fd p0 =
  let ms1 = [1; 2; 3]
  and ms2 = [10; 20; 30]
  and ms3 = [100; 200; 300]
  in
  ambk p0 ms1 @@ fun p1 m1 ->
  lswrite_i fd m1 >>= fun () ->
    ambk p1 ms2 @@ fun p2 m2 ->
    lswrite_i fd m2 >>= fun () ->
      ambk p2 ms3 @@ fun p3 m3 ->
      lswrite_i fd m3 >>= fun () ->
        lswrite fd "END\n"

module K2 = struct

  type 'a m =
      Pure   : 'a -> 'a m
    | Impure : 'x list * ('x -> 'a t) -> 'a m
  and 'a t = 'a m Lwt.t

  let return x = Lwt.return (Pure x)

  module Infix = struct
    let rec (>>=) : type a b. a t -> (a -> b t) -> b t =
      fun a fb -> Lwt.(>>=) a @@ function
          Pure x          -> fb x
        | Impure (amb, k) -> Lwt.return @@ Impure (amb, fun x -> k x >>= fb)
    let (>|=) a f = a >>= fun x -> return (f x)
  end

  let amb xs = Lwt.return @@ Impure (xs, return)

  let lift m = Lwt.(m >|= fun x -> Pure x)

  let rec execute t = Lwt.(t >>= function
    | Pure _          -> return ()
    | Impure (amb, k) -> Lwt_list.iter_s (fun x -> k x |> execute) amb
  )

  include Infix

  let ambp p xs =
    Fmt.(pr "* amb: pid: %d; alts %a\n%!" (Pctrl.pid p) (Dump.list int) xs);
    let spawn () =
      Lwt.(Pctrl.(clone p >>= fun q ->
        Fmt.pr "+ amb: created: %d -> %d\n%!" (Pctrl.pid p) (Pctrl.pid q);
        unfreeze q >|= fun () -> q)) in
    amb xs >>= fun x -> lift (spawn ()) >|= fun q -> (q, x)
end


let explore2 fd p0 =
  let open K2 in
  let write p m =
    Fmt.(pr "- w: hoping to send: [%d] -> %d\n%!" m (Pctrl.pid p));
    lift (lswrite_i fd m) in
  let ms1 = [1; 2; 3]
  and ms2 = [10; 20; 30]
  and ms3 = [100; 200; 300]
  in
  ambp p0 ms1 >>= fun (p1, m1) ->
    write p1 m1 >>= fun () ->
    ambp p1 ms2 >>= fun (p2, m2) ->
      write p2 m2 >>= fun () ->
      ambp p2 ms3 >>= fun (p3, m3) ->
        write p3 m3 >>= fun () ->
        lift @@ lswrite fd "END\n"


let main () =
  let (s1, s2) = Unix.(socketpair PF_UNIX SOCK_STREAM 0) in
  let ctx =
    Pctrl.create ~timeout:5.
      ~env:[(envvar, string_of_int (Fd.to_int s2))] () in
  let fd = Lwt_unix.of_unix_file_descr s1 in
  Pctrl.start ctx "./demo1slave.native" [] >>= fun p0 ->
    K2.execute @@ explore2 fd p0

let () = Lwt_main.run @@ main ()
