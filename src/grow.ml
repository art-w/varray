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
    { mutable small : 'a V.t
    ; mutable large : 'a V.t
    }

  let length t = V.length t.small + V.length t.large

  let capacity_for = function
    | n when n <= 0 -> 1
    | n -> pow2 (log2 n /^ V.depth)

  let empty () =
    { small = V.create ~capacity:1 ; large = V.empty }

  let is_empty t = V.is_empty t.small && V.is_empty t.large

  let make n x =
    let capacity = capacity_for n in
    { small = V.make ~capacity n x ; large = V.empty }

  let init n f =
    let capacity = capacity_for n in
    { small = V.init ~capacity ~offset:0 n f ; large = V.empty }

  let get t i =
    let j = i - V.length t.small in
    if j < 0
    then V.get t.small i
    else V.get t.large j

  let set t i x =
    let j = i - V.length t.small in
    if j < 0
    then V.set t.small i x
    else V.set t.large j x

  let do_swap t =
    t.small <- t.large ;
    t.large <- V.empty

  let swap t =
    if V.is_empty t.small && not (V.is_empty t.large)
    then do_swap t

  let is_growing t = length t >= V.capacity t.small

  let pow2_depth = pow2 V.depth

  let incr_capacity t =
    swap t ;
    if is_growing t
    then begin
      if V.length t.large > 0
      then begin
        assert (not (V.is_empty t.small)) ;
        let tl = V.pop_back t.small in
        V.push_front t.large tl ;
        if V.is_empty t.small
        then do_swap t
      end
      else begin
        assert (V.length t.large = 0) ;
        let tl = V.pop_back t.small in
        let capacity = 2 * V.root_capacity t.small in
        t.large <- V.make ~capacity 1 tl
      end
    end


  let insert_at t i x =
    incr_capacity t ;
    match i - V.length t.small with
    | j when j <= 0 ->
        V.insert_at t.small i x ;
        incr_capacity t
    | j ->
        V.insert_at t.large j x

  let is_shrinking t = length t * pow2_depth < V.root_capacity t.small

  let decr_capacity t =
    swap t ;
    if is_shrinking t
    then begin
      if not (V.is_empty t.large)
      then begin
        assert (not (V.is_full t.small)) ;
        V.push_back t.small (V.pop_front t.large)
      end
      else if not (V.is_empty t.small) && V.root_capacity t.small > 1
      then begin
        let capacity = V.root_capacity t.small /^ 2 in
        let hd = V.pop_front t.small in
        t.large <- t.small ;
        t.small <- V.make ~capacity 1 hd ;
      end
    end

  let pop_at t i =
    decr_capacity t ;
    match i - V.length t.small with
    | j when j < 0 ->
        let x = V.pop_at t.small i in
        decr_capacity t ;
        x
    | j ->
        V.pop_at t.large j

  let delete_at t i = ignore (pop_at t i)

  let push_back t x =
    incr_capacity t ;
    if V.is_empty t.large
    then V.push_back t.small x
    else V.push_back t.large x

  let push_front t x =
    incr_capacity t ;
    V.push_front t.small x ;
    incr_capacity t

  let pop_front t =
    decr_capacity t ;
    let x = V.pop_front t.small in
    decr_capacity t ;
    x

  let pop_back t =
    decr_capacity t ;
    if V.is_empty t.large
    then V.pop_back t.small
    else V.pop_back t.large

end
