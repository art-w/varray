let size = 1_000_000
let input = Array.init size (fun i -> i)

let bench f =
  let t0 = Unix.gettimeofday () in
  ignore (f ()) ;
  let t1 = Unix.gettimeofday () in
  t1 -. t0

module type ARRAY = sig
  type 'a t
  val get : 'a t -> int -> 'a
  val set : 'a t -> int -> 'a -> unit
  val length : 'a t -> int
  val of_array : 'a array -> 'a t
end

let bench_get (module Array : ARRAY) =
  let t = Array.of_array input in
  let len = Array.length t in
  let expected = len * (len - 1) / 2 in
  bench begin fun () ->
    let total = ref 0 in
    for i = 0 to len - 1 do
      total := !total + t.(i)
    done ;
    assert (!total = expected)
  end

let bench_set (module Array : ARRAY) =
  let t = Array.of_array input in
  bench begin fun () ->
    for i = 0 to Array.length t - 1 do
      t.(i) <- i
    done
  end

module Stdarray = struct include Array let of_array t = t end
module V1 = Varray.Circular
module V2 = Varray.Root (V1)
module V3 = Varray.Root (V2)
module V4 = Varray.Root (V3)
module V5 = Varray.Root (V4)
module V6 = Varray.Root (V5)

let all =
  [ (module Stdarray : ARRAY)
  ; (module V1 : ARRAY)
  ; (module V2 : ARRAY)
  ; (module V3 : ARRAY)
  ; (module V4 : ARRAY)
  ; (module V5 : ARRAY)
  ; (module V6 : ARRAY)
  ]

let results_get = List.map bench_get all
let results_set = List.map bench_set all

let table column results =
  let base = List.hd results in
  let norm x = x /. base in
  List.iteri
    (fun i dt ->
      let s = Printf.sprintf " %.0fx " (norm dt) in
      let missing = String.length column.(i + 1) - String.length s in
      let padding = String.make missing ' ' in
      Printf.printf "%s%s|" padding s
    )
    results ;
  Printf.printf "\n%!"

let () =
  let columns =
    Array.init (1 + List.length all)
      (function
        | 0 -> "     "
        | 1 -> " Array "
        | 2 -> " Circular "
        | 3 -> " Root "
        | n -> Printf.sprintf " Root<sup>%i</sup> " (n - 2))
  in
  Array.iter (Printf.printf "|%s") columns ;
  Printf.printf "|\n%!" ;
  Array.iter
    (fun n ->
      let len = String.length n - 1 in
      Printf.printf "|%s:" (String.make len '-'))
    columns ;
  Printf.printf "|\n%!" ;
  Printf.printf "| get |" ;
  table columns results_get ;
  Printf.printf "| set |" ;
  table columns results_set
