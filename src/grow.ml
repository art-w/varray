let ( /^ ) a b = 1 + (a - 1) / b

let rec log2 = function
  | 0 | 1 -> 0
  | n -> 1 + log2 (n /^ 2)

let pow2 n = 1 lsl n

module Make (V : Varray_sig.TIER)
: Varray_sig.VARRAY with type 'a elt = 'a V.elt and type 'a array = 'a V.array
= struct

  module Tier = V

  module Array = V.Array
  type 'a elt = 'a V.elt
  type 'a array = 'a V.array

  type 'a t =
    { mutable lc : int
    ; mutable small : 'a V.t
    ; mutable large : 'a V.t
    }

  let length t =
    V.length ~lc:t.lc t.small
    + V.length ~lc:(t.lc + 1) t.large

  let lc_for = function
    | n when n <= 0 -> 1
    | n -> log2 n /^ V.depth

  let empty () =
    { lc = 0
    ; small = V.create ~capacity:1
    ; large = V.empty
    }

  let is_empty t = V.is_empty t.small && V.is_empty t.large

  let make n x =
    let lc = lc_for n in
    { lc
    ; small = V.make ~lc n x
    ; large = V.empty
    }

  let init n f =
    let lc = lc_for n in
    { lc
    ; small = V.init ~lc ~offset:0 n f
    ; large = V.empty
    }

  let get t i =
    let lc = t.lc in
    let length_small = V.length ~lc t.small in
    match i - length_small with
    | j when i >= 0 && j < 0 ->
        V.get ~lc t.small i
    | j when j >= 0 && j < V.length ~lc:(lc + 1) t.large ->
        V.get ~lc:(lc + 1) t.large j
    | _ ->
        invalid_arg "index out of bounds"

  let set t i x =
    let lc = t.lc in
    let length_small = V.length ~lc t.small in
    match i - length_small with
    | j when i >= 0 && j < 0 ->
        V.set ~lc t.small i x
    | j when j >= 0 && j < V.length ~lc:(lc + 1) t.large ->
        V.set ~lc:(lc + 1) t.large j x
    | _ ->
        invalid_arg "index out of bounds"

  let do_swap t =
    t.lc <- 1 + t.lc ;
    t.small <- t.large ;
    t.large <- V.empty

  let swap t =
    if V.is_empty t.small && not (V.is_empty t.large)
    then do_swap t

  let is_growing t = length t >= V.capacity ~lc:t.lc t.small

  let pow2_depth = pow2 V.depth

  let incr_capacity t =
    swap t ;
    if is_growing t
    then begin
      if not (V.is_empty t.large)
      then begin
        assert (not (V.is_empty t.small)) ;
        let lc = t.lc in
        let tl = V.pop_back ~lc t.small in
        let lc = t.lc + 1 in
        V.push_front ~lc t.large tl ;
        if V.is_empty t.small
        then do_swap t
      end
      else begin
        let tl = V.pop_back ~lc:t.lc t.small in
        let lc = 1 + t.lc in
        t.large <- V.make ~lc 1 tl
      end
    end

  let insert_at t i x =
    incr_capacity t ;
    match i - V.length ~lc:t.lc t.small with
    | j when j <= 0 ->
        V.insert_at ~lc:t.lc t.small i x ;
        incr_capacity t
    | j ->
        V.insert_at ~lc:(t.lc + 1) t.large j x

  let is_shrinking t =
    length t * pow2_depth < V.capacity ~lc:t.lc t.small

  let decr_capacity t =
    swap t ;
    if is_shrinking t
    then begin
      if not (V.is_empty t.large)
      then begin
        let lc = t.lc in
        assert (not (V.is_full ~lc t.small)) ;
        V.push_back ~lc t.small (V.pop_front ~lc:(lc + 1) t.large)
      end
      else if not (V.is_empty t.small) && t.lc > 1 (* V.root_capacity ~lc:t.lc t.small > 1 *)
      then begin
        let lc = t.lc in
        let hd = V.pop_front ~lc t.small in
        let lc = t.lc - 1 in
        t.lc <- lc ;
        t.large <- t.small ;
        t.small <- V.make ~lc 1 hd ;
      end
    end

  let pop_at t i =
    decr_capacity t ;
    match i - V.length ~lc:t.lc t.small with
    | j when j < 0 ->
        let x = V.pop_at ~lc:t.lc t.small i in
        decr_capacity t ;
        x
    | j ->
        V.pop_at ~lc:(t.lc + 1) t.large j

  let delete_at t i = ignore (pop_at t i)

  let push_back t x =
    incr_capacity t ;
    let lc = t.lc in
    if V.is_empty t.large
    then V.push_back ~lc t.small x
    else V.push_back ~lc:(lc + 1) t.large x

  let push_front t x =
    incr_capacity t ;
    let lc = t.lc in
    V.push_front ~lc t.small x ;
    incr_capacity t

  let pop_front t =
    decr_capacity t ;
    let x = V.pop_front ~lc:t.lc t.small in
    decr_capacity t ;
    x

  let pop_back t =
    decr_capacity t ;
    if V.is_empty t.large
    then V.pop_back ~lc:t.lc t.small
    else V.pop_back ~lc:(t.lc + 1) t.large

end
