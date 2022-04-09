(* module V = Varray *)
module V = Varray.Root (Varray.Root (Varray.Circular))

(*
module Naive = struct
  include Varray.Circular
  let check varray t =
    assert (length t = V.length varray) ;
    iteri (fun i v -> assert (V.get varray i = v)) t
end
*)

module Naive = struct
  type 'a elt = 'a
  type 'a t = 'a list ref

  let check varray t =
    assert (List.length !t = V.length varray) ;
    List.iteri (fun i v -> assert (V.get varray i = v)) !t

  let make n x = ref (List.init n (fun _ -> x))

  let push_front t x = t := x :: !t

  let push_back t x = t := !t @ [x]

  let pop_front t = match !t with
    | [] -> raise Not_found
    | x :: xs -> t := xs ; x

  let pop_back t = match List.rev !t with
    | [] -> raise Not_found
    | x :: xs -> t := List.rev xs ; x

  let insert_at t i x =
    let rec go i = function
      | xs when i = 0 -> x :: xs
      | x :: xs -> x :: go (i - 1) xs
      | [] -> invalid_arg "index out of bounds"
    in
    t := go i !t

  let pop_at t i =
    let rec go acc i = function
      | [] -> invalid_arg "index out of bounds"
      | x :: xs when i = 0 -> x, List.rev_append acc xs
      | x :: xs -> go (x::acc) (i - 1) xs
    in
    let elt, xs = go [] i !t in
    t := xs ;
    elt
end

open Monolith

let element = sequential ()

let check model = Naive.check model, constant "check"

let varray = declare_abstract_type ~check ()

let length = le 1024

let () =
  let spec = length ^> element ^> varray in
  declare "make" spec V.make Naive.make ;

  let spec = varray ^!> element in
  declare "pop_front" spec V.pop_front Naive.pop_front ;
  declare "pop_back"  spec V.pop_back  Naive.pop_back ;

  let spec = varray ^> element ^> unit in
  declare "push_front" spec V.push_front Naive.push_front ;
  declare "push_back"  spec V.push_back  Naive.push_back ;

  let spec = varray ^>> (fun v -> lt (V.length v) ^> element) in
  declare "pop_at" spec V.pop_at Naive.pop_at ;

  let spec = varray ^>> (fun v -> le (V.length v) ^> element ^> unit) in
  declare "insert_at" spec V.insert_at Naive.insert_at ;

  main 5
