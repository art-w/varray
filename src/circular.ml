module Make (Arg : Varray_sig.ARRAY)
: sig
    include Varray_sig.TIER with type 'a Array.t = 'a Arg.t
                             and type 'a Array.elt = 'a Arg.elt
    val grow_head : 'a t -> unit
    val grow_tail : 'a t -> unit
    val unsafe_pop_front : 'a t -> unit
    val unsafe_pop_back : 'a t -> unit
end
= struct

  module Array = Arg
  type 'a elt = 'a Array.elt
  type 'a array = 'a Array.t

  type 'a t =
    { mutable head: int
    ; mutable length: int
    ; buffer: 'a Array.t
    }

  let depth = 1

  let length t = t.length

  let is_empty t = length t = 0

  let capacity t = Array.length t.buffer
  let root_capacity t = Array.length t.buffer

  let is_full t = t.length = capacity t

  let empty =
    Obj.magic
      { head = 0
      ; length = 0
      ; buffer = Array.empty
      }

  let create ~capacity =
    { head = 0
    ; length = 0
    ; buffer = Array.create capacity
    }

  let make ~capacity n x =
    assert (capacity > 0) ;
    let buffer = Array.create capacity in
    for i = 0 to n - 1 do
      Array.set buffer i x
    done ;
    { head = 0
    ; length = n
    ; buffer
    }

  let init ~capacity ~offset n f =
    assert (capacity > 0) ;
    let buffer = Array.create capacity in
    for i = 0 to n - 1 do
      let x = f (i + offset) in
      Array.set buffer i x
    done ;
    { head = 0
    ; length = n
    ; buffer
    }

  let index t i = (t.head + i) mod capacity t
  let index_last t = index t (t.length - 1)

  let get t i =
    assert (i >= 0 && i < length t) ;
    t.buffer.(index t i)

  let set t i x =
    assert (i >= 0 && i < length t) ;
    t.buffer.(index t i) <- x

  let shift_right t j =
    let tail = index t t.length in
    if j <= tail
    then Array.blit t.buffer j t.buffer (j + 1) (tail - j)
    else begin
      let last = t.buffer.(capacity t - 1) in
      Array.blit t.buffer j t.buffer (j + 1) (capacity t - j - 1) ;
      Array.blit t.buffer 0 t.buffer 1 tail ;
      t.buffer.(0) <- last
    end

  let shift_left t j =
    let head = t.head in
    if j >= head
    then begin
      let prev = (head - 1 + capacity t) mod (capacity t) in
      t.buffer.(prev) <- t.buffer.(head) ;
      Array.blit t.buffer (head + 1) t.buffer head (j - head)
    end
    else begin
      Array.blit t.buffer head t.buffer (head - 1) (capacity t - head) ;
      t.buffer.(capacity t - 1) <- t.buffer.(0) ;
      Array.blit t.buffer 1 t.buffer 0 j ;
    end

  let head_left t =
    t.head <- (t.head - 1 + capacity t) mod (capacity t)

  let grow_tail t =
    t.length <- t.length + 1

  let grow_head t =
    assert (length t < capacity t) ;
    head_left t ;
    grow_tail t

  let push_front t x =
    assert (length t < capacity t) ;
    grow_head t ;
    t.buffer.(t.head) <- x

  let push_back t x =
    assert (length t < capacity t) ;
    grow_tail t ;
    t.buffer.(index_last t) <- x

  let make_room t i =
    assert (length t < capacity t) ;
    if 2 * i >= t.length
    then begin
      let j = index t i in
      shift_right t j ;
      grow_tail t
    end
    else begin
      let j = index t i in
      shift_left t j ;
      grow_head t
    end

  let insert_at t i x =
    assert (i >= 0 && i <= length t) ;
    assert (length t < capacity t) ;
    make_room t i ;
    set t i x


  let shrink_tail t tail =
    Array.erase_at t.buffer tail ;
    t.length <- t.length - 1

  let shrink_head t head =
    Array.erase_at t.buffer head ;
    t.head <- (head + 1) mod (capacity t) ;
    t.length <- t.length - 1


  let shrink_prev_head t =
    if t.length + 1 < capacity t
    then let prev = (t.head - 1 + capacity t) mod (capacity t) in
         Array.erase_at t.buffer prev

  let shrink_next_tail t =
    if t.length + 1 < capacity t
    then let next = (t.head + t.length) mod (capacity t) in
         Array.erase_at t.buffer next

  let unsafe_pop_front t =
    shrink_prev_head t ;
    t.head <- (t.head + 1) mod (capacity t) ;
    t.length <- t.length - 1

  let unsafe_pop_back t =
    shrink_next_tail t ;
    t.length <- t.length - 1

  let delete_right t j =
    let tail = index_last t in
    if j = tail
    then ()
    else if j < tail
    then Array.blit t.buffer (j + 1) t.buffer j (tail - j)
    else begin
      Array.blit t.buffer (j + 1) t.buffer j (capacity t - 1 - j) ;
      t.buffer.(capacity t - 1) <- t.buffer.(0) ;
      Array.blit t.buffer 1 t.buffer 0 tail
    end ;
    shrink_tail t tail

  let delete_left t j =
    let head = t.head in
    if j = head
    then ()
    else if head < j
    then Array.blit t.buffer head t.buffer (head + 1) (j - head)
    else begin
        let last = t.buffer.(capacity t - 1) in
        Array.blit t.buffer head t.buffer (head + 1) (capacity t - 1 - head) ;
        Array.blit t.buffer 0 t.buffer 1 j ;
        t.buffer.(0) <- last ;
    end ;
    shrink_head t head

  let delete_at t i =
    let j = index t i in
    if 2 * i >= t.length
    then delete_right t j
    else delete_left t j

  let pop_at t i =
    let x = get t i in
    delete_at t i ;
    x

  let pop_front t =
    assert (length t > 0) ;
    let x = t.buffer.(t.head) in
    shrink_head t t.head ;
    x

  let pop_back t =
    assert (length t > 0) ;
    let tail = index_last t in
    let x = t.buffer.(tail) in
    shrink_tail t tail ;
    x

  let push_front_pop_back t x =
    let tail = index_last t in
    let last = t.buffer.(tail) in
    head_left t ;
    t.buffer.(t.head) <- x ;
    last

  let push_back_pop_front t x =
    let first = t.buffer.(t.head) in
    t.head <- (t.head + 1) mod (capacity t) ;
    let last = index_last t in
    t.buffer.(last) <- x ;
    first

end
