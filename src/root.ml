let pow2 x = 1 lsl x

module Make (V : Varray_sig.TIER)
: Varray_sig.TIER with module Array = V.Array
= struct

  module Buffer = Circular.Make (struct
    include Array
    type 'a elt = 'a V.t
    type 'a t = 'a elt array
    let get = Array.unsafe_get
    let set = Array.unsafe_set
    let empty : type a. unit -> a t = fun () -> [| |]
    let create n = Array.make n (V.empty ())
    let erase_at t i = set t i (V.empty ())
  end)

  module Array = V.Array
  type 'a elt = 'a V.elt
  type 'a array = 'a V.array

  type 'a t =
    { mutable length : int
    ; mutable first : 'a V.t
    ; mutable rows : 'a Buffer.t
    }

  let empty () =
    { length = 0 ; first = V.empty () ; rows = Buffer.empty () }

  let is_empty t = t.length = 0

  let depth = V.depth + 1

  let sector_length ~lc = pow2 (lc * V.depth)

  let capacity ~lc = pow2 (lc * depth)

  let length t = t.length

  let is_full ~lc t = length t = capacity ~lc

  let root_capacity t = Buffer.root_capacity t.rows

  let create ~capacity =
    { length = 0
    ; first = V.empty ()
    ; rows = Buffer.create ~capacity
    }

  let make ~lc n x =
    let capacity = pow2 lc in
    assert (capacity > 0) ;
    let sector_length = sector_length ~lc in
    let remaining, nb_full_parts =
      match n mod sector_length with
      | 0 when n < sector_length -> n, 0
      | 0 -> sector_length, n / sector_length - 1
      | rest -> rest, n / sector_length
    in
    let t = create ~capacity in
    t.length <- n ;
    Buffer.set_length t.rows nb_full_parts ;
    assert (remaining <= n) ;
    assert ((n > 0) = (remaining > 0)) ;
    assert (n = remaining + sector_length * nb_full_parts) ;
    t.first <- V.make ~lc remaining x ;
    for i = 0 to nb_full_parts - 1 do
      let row = V.make ~lc sector_length x in
      Buffer.set ~lc t.rows i row
    done ;
    t

  let init ~lc ~offset n f =
    let capacity = pow2 lc in
    assert (capacity > 0) ;
    let sector_length = sector_length ~lc in
    let remaining, nb_full_parts =
      match n mod sector_length with
      | 0 when n < sector_length -> n, 0
      | 0 -> sector_length, n / sector_length - 1
      | rest -> rest, n / sector_length
    in
    let t = create ~capacity in
    t.length <- n ;
    Buffer.set_length t.rows nb_full_parts ;
    assert (remaining <= n) ;
    assert ((n > 0) = (remaining > 0)) ;
    assert (n = remaining + sector_length * nb_full_parts) ;
    t.first <- V.init ~lc ~offset remaining f ;
    let offset = offset + remaining in
    for i = 0 to nb_full_parts - 1 do
      let offset = offset + i * sector_length in
      let row = V.init ~lc ~offset sector_length f in
      Buffer.set ~lc t.rows i row
    done ;
    t

  let has_capacity child = V.root_capacity child > 0

  let create_child ~lc t i x =
    let row = V.make ~lc 1 x in
    Buffer.set ~lc t.rows i row

  let initialize ~lc t =
    assert (Buffer.root_capacity t.rows = pow2 lc)

  let push_front_new ~lc t x =
    initialize ~lc t ;
    Buffer.grow_head ~lc t.rows ;
    let fst = Buffer.get ~lc t.rows 0 in
    assert (V.is_empty fst) ;
    assert (V.is_full ~lc t.first) ;
    Buffer.set ~lc t.rows 0 t.first ;
    t.first <-
      if has_capacity fst
      then (V.push_front ~lc fst x ; fst)
      else V.make ~lc 1 x

  let push_front ~lc t x =
    assert (not (is_full ~lc t)) ;
    begin
      if is_empty t
      then if has_capacity t.first
           then V.push_front ~lc t.first x
           else t.first <- V.make ~lc 1 x
      else if V.is_full ~lc t.first
           then push_front_new ~lc t x
           else V.push_front ~lc t.first x
    end ;
    t.length <- t.length + 1 ;
    assert (V.length t.first > 0)

  let push_back_new ~lc t x =
    initialize ~lc t ;
    let last_idx = Buffer.length t.rows in
    Buffer.grow_tail t.rows ;
    let fst = Buffer.get ~lc t.rows last_idx in
    assert (V.is_empty fst) ;
    if has_capacity fst
    then V.push_back ~lc fst x
    else create_child ~lc t last_idx x

  let push_back ~lc t x =
    assert (not (is_full ~lc t)) ;
    let n = Buffer.length t.rows - 1 in
    begin
      if n < 0
      then if not (has_capacity t.first) || V.is_full ~lc t.first
           then push_back_new ~lc t x
           else V.push_back ~lc t.first x
      else let tail = Buffer.get ~lc t.rows n in
           if V.is_full ~lc tail
           then push_back_new ~lc t x
           else V.push_back ~lc tail x
    end ;
    t.length <- t.length + 1

  let clean_front ~lc t =
    if V.is_empty t.first && Buffer.length t.rows > 0
    then t.first <- Buffer.pop_front ~lc t.rows

  let pop_front ~lc t =
    let first = t.first in
    let v = V.pop_front ~lc first in
    clean_front ~lc t ;
    t.length <- t.length - 1 ;
    v

  let clean_back ~lc t last =
    if V.is_empty last
    then begin
      assert (Buffer.length t.rows > 0) ;
      Buffer.unsafe_pop_back ~lc t.rows
    end

  let pop_back ~lc t =
    t.length <- t.length - 1 ;
    let i = Buffer.length t.rows - 1 in
    if i < 0
    then begin
      let x = V.pop_back ~lc t.first in
      clean_front ~lc t ;
      x
    end
    else begin
      let last = Buffer.get ~lc t.rows i in
      let v = V.pop_back ~lc last in
      clean_back ~lc t last ;
      v
    end

  let indexes' ~lc t i =
    let first = t.first in
    let first_len = V.length first in
    if i < first_len
    then 0, i
    else let i = i - first_len in
         let lcd = lc * V.depth in
         let j = 1 + i lsr lcd in
         let i = i land (pow2 lcd - 1) in
         j, i

  let indexes ~lc t i =
    if i = 0
    then 0, 0
    else indexes' ~lc t i

  let buffer_get ~lc t j =
    if j = 0
    then t.first
    else Buffer.get ~lc t.rows (j - 1)

  let get ~lc t i =
    assert (i >= 0 && i < length t) ;
    let j, i = indexes' ~lc t i in
    let row = buffer_get ~lc t j in
    V.get ~lc row i

  let set ~lc t i x =
    assert (i >= 0 && i < length t) ;
    let j, i = indexes' ~lc t i in
    let row = buffer_get ~lc t j in
    V.set ~lc row i x

  let collapse ~lc t j row =
    let len = Buffer.length t.rows in
    if 2 * j < len
    then begin
      let first = t.first in
      let v = ref (V.pop_back ~lc first) in
      for k = 0 to j - 2 do
        let row = Buffer.get ~lc t.rows k in
        v := V.push_front_pop_back ~lc row !v
      done ;
      V.push_front ~lc row !v ;
      clean_front ~lc t ;
      assert (V.length t.first > 0) ;
      for i = 0 to Buffer.length t.rows - 2 do
        let row = Buffer.get ~lc t.rows i in
        assert (V.length row = sector_length ~lc)
      done

    end
    else begin
      let len = len - 1 in
      let last = Buffer.get ~lc t.rows len in
      let v = ref (V.pop_front ~lc last) in
      for k = len - 1 downto j do
        let row = Buffer.get ~lc t.rows k in
        v := V.push_back_pop_front ~lc row !v ;
      done ;
      V.push_back ~lc row !v ;
      clean_back ~lc t last
    end

  let pop_at ~lc t i =
    assert (i >= 0 && i < length t) ;
    let j, i = indexes ~lc t i in
    let row = buffer_get ~lc t j in
    assert (j >= 0 && j <= Buffer.length t.rows) ;
    assert (i >= 0 && i <  V.length row) ;
    let x = V.pop_at ~lc row i in
    t.length <- t.length - 1 ;
    if j = 0
    then clean_front ~lc t
    else if j >= Buffer.length t.rows
    then clean_back ~lc t row
    else collapse ~lc t j row ;
    x

  let push_front_pop_back ~lc t x =
    let y = pop_back ~lc t in
    push_front ~lc t x ;
    y

  let push_back_pop_front ~lc t x =
    let y = pop_front ~lc t in
    push_back ~lc t x ;
    y

  let insert_at ~lc t i x =
    assert (not (is_full ~lc t)) ;
    if i = 0
    then push_front ~lc t x
    else begin
      let j, i = indexes ~lc t i in
      let len = Buffer.length t.rows in
      if j = 0
      then begin
        t.length <- t.length + 1 ;
        if V.is_full ~lc t.first
        then begin
          assert (i > 0) ;
          let y = V.pop_front ~lc t.first in
          V.insert_at ~lc t.first (i - 1) x ;
          push_front_new ~lc t y
        end
        else V.insert_at ~lc t.first i x
      end
      else if j > len
      then push_back ~lc t x
      else begin
        let j = j - 1 in
        let row = Buffer.get ~lc t.rows j in
        if 2 * j < len
        then begin
          let v =
            if i = 0
            then x
            else begin
              let y = V.pop_front ~lc row in
              V.insert_at ~lc row (i - 1) x ;
              y
            end
          in
          let v = ref v in
          for k = j - 1 downto 0 do
            let row = Buffer.get ~lc t.rows k in
            v := V.push_back_pop_front ~lc row !v
          done ;
          v := V.push_back_pop_front ~lc t.first !v ;
          push_front ~lc t !v
        end
        else begin
          let v =
            if i = V.length row
            then x
            else begin
              let y = V.pop_back ~lc row in
              V.insert_at ~lc row i x ;
              y
            end
          in
          let v = ref v in
          for k = j + 1 to len - 1 do
            let row = Buffer.get ~lc t.rows k in
            v := V.push_front_pop_back ~lc row !v
          done ;
          push_back ~lc t !v
        end
      end
    end

end
