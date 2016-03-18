open Lwt.Infix
open Common

let rec (--) a b = if a > b then [] else a :: succ a -- b

let send_range fd a b =
  (a -- b) |> Lwt_list.map_s @@ fun n ->
    lwrite fd (string_of_int n ^ "\n")

let lread fd =
  let buf = Bytes.create 256 in
  Lwt_unix.read fd buf 0 256 >|= Bytes.sub buf 0

let amb p xs =
  xs |> Lwt_list.map_s @@ fun x ->
    Pctrl.clone p >|= fun q -> (q, x)

let ambk p xs k =
  xs |> Lwt_list.iter_s @@ fun x ->
    Pctrl.clone p >>= fun q -> k q x

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
      Lwt.(Pctrl.(clone p >|= fun q ->
        Fmt.pr "+ amb: created: %d -> %d\n%!" (Pctrl.pid p) (Pctrl.pid q); q)) in
    amb xs >>= fun x -> lift (spawn ()) >|= fun q -> (q, x)
end


let explore2 p0 =
  let open K2 in
  let wrstep p m =
    lift Lwt.(
      lwrite (Pctrl.fd p) (string_of_int m ^ "\n") >>= fun () ->
        lread (Pctrl.fd p) >>= function
          | "." -> return_unit
          | _   -> fail (Failure "out of sync.") ) in
  let ms1 = [1; 2; 3; 4]
  and ms2 = [10; 20; 30; 40]
  and ms3 = [100; 200; 300; 400]
  and ms4 = [1000; 2000; 3000; 4000]
  in
  ambp p0 ms1 >>= fun (p1, m1) ->
    wrstep p1 m1 >>= fun () ->
    ambp p1 ms2 >>= fun (p2, m2) ->
      wrstep p2 m2 >>= fun () ->
      ambp p2 ms3 >>= fun (p3, m3) ->
        wrstep p3 m3 >>= fun () ->
          ambp p3 ms4 >>= fun (p4, m4) ->
            wrstep p4 m4 >>= fun () ->
              lift Lwt.(
                lwrite (Pctrl.fd p4) "END\n" >>= fun _ -> Pctrl.wait_dead p4)

let main2 () =
  let ctx = Pctrl.create ~timeout:5. () in
  Pctrl.start ctx "./demo1slave.native" [] >>= fun p0 ->
    K2.execute @@ explore2 p0

let variate p0 msgs k =
  let open Lwt in
  Lwt_list.iter_p (fun m ->
    Pctrl.clone p0 >>= fun p -> k p m)
  msgs >>= fun () -> Pctrl.terminate p0

let explore3 p0 =
  let open Lwt in
  let wrstep p m =
    let fd = Pctrl.fd p in
    lwrite fd (string_of_int m ^ "\n") >>= fun () ->
      lread fd >>= function
        | "." -> return_unit
        | _   -> fail (Failure "out of sync.") in
  let ms1 = [1; 2; 3; 4]
  and ms2 = [10; 20; 30; 40]
  and ms3 = [100; 200; 300; 400]
  and ms4 = [1000; 2000; 3000; 4000] in
  variate p0 ms1 @@ fun p1 m1 ->
    wrstep p1 m1 >>= fun () ->
      variate p1 ms2 @@ fun p2 m2 ->
        wrstep p2 m2 >>= fun () ->
          variate p2 ms3 @@ fun p3 m3 ->
            wrstep p3 m3 >>= fun () ->
              variate p3 ms4 @@ fun p4 m4 ->
                wrstep p4 m4 >>= fun () ->
                  lwrite (Pctrl.fd p4) "END\n" >>= fun _ -> Pctrl.wait_dead p4

let main3 () =
  let ctx = Pctrl.create ~timeout:5. () in
  Pctrl.start ctx "./demo1slave.native" [] >>= explore3

let () = Lwt_main.run @@ main3 ()
