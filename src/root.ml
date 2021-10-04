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
    let empty : type a. a t = [| |]
    let create n = Array.make n V.empty
    let erase_at t i = set t i V.empty
  end)

  module Array = V.Array
  type 'a elt = 'a V.elt
  type 'a array = 'a V.array

  type 'a t = 'a Buffer.t

  let empty : type a. a t = Buffer.empty

  let is_empty t = Buffer.is_empty t

  let depth = V.depth + 1

  let root_capacity ~lc t = Buffer.root_capacity ~lc t - 1

  let sector_length ~lc = pow2 (lc * V.depth)

  let capacity ~lc _t = pow2 (lc * depth)

  let length ~lc t = match Buffer.length ~lc t with
    | 0 -> 0
    | 1 -> V.length ~lc (Buffer.get ~lc t 0)
    | n ->
        let first = Buffer.get ~lc t 0 in
        let last = Buffer.get ~lc t (n - 1) in
        V.length ~lc first
        + V.length ~lc last
        + sector_length ~lc * (n - 2)

  let is_full ~lc t = length ~lc t = capacity ~lc t

  let create ~capacity =
    Buffer.create ~capacity:(capacity + 1)

  let make ~lc n x =
    let capacity = pow2 lc in
    assert (capacity > 0) ;
    let sector_length = sector_length ~lc in
    let nb_full_parts = n / sector_length in
    let remaining = n - (sector_length * nb_full_parts) in
    let nb = if remaining > 0 then 1 + nb_full_parts else nb_full_parts in
    let rows = create ~capacity in
    Buffer.set_length rows nb ;
    for i = 0 to nb_full_parts - 1 do
      let row = V.make ~lc sector_length x in
      Buffer.set ~lc rows i row
    done ;
    if remaining > 0
    then begin
      let row = V.make ~lc remaining x in
      Buffer.set ~lc rows nb_full_parts row ;
    end ;
    rows

  let init ~lc ~offset n f =
    let capacity = pow2 lc in
    assert (capacity > 0) ;
    let sector_length = sector_length ~lc in
    let nb_full_parts = n / sector_length in
    let full = sector_length * nb_full_parts in
    let remaining = n - full in
    let nb = if remaining > 0 then 1 + nb_full_parts else nb_full_parts in
    let rows = create ~capacity in
    Buffer.set_length rows nb ;
    for i = 0 to nb_full_parts - 1 do
      let offset = offset + i * sector_length in
      let row = V.init ~lc ~offset sector_length f in
      Buffer.set ~lc rows i row
    done ;
    if remaining > 0
    then begin
      let row = V.init ~lc ~offset:(offset + full) remaining f in
      Buffer.set ~lc rows nb_full_parts row ;
    end ;
    rows

  let has_capacity ~lc t = V.root_capacity ~lc t > 0

  let create_child ~lc t i x =
    let row = V.make ~lc 1 x in
    Buffer.set ~lc t i row

  let push_front_new ~lc t x =
    Buffer.grow_head ~lc t ;
    let fst = Buffer.get ~lc t 0 in
    if has_capacity ~lc fst
    then V.push_front ~lc fst x
    else create_child ~lc t 0 x

  let push_front ~lc t x =
    assert (length ~lc t < capacity ~lc t) ;
    if Buffer.is_empty t
    then push_front_new ~lc t x
    else let head = Buffer.get ~lc t 0 in
         if V.is_full ~lc head
         then push_front_new ~lc t x
         else V.push_front ~lc head x

  let push_back_new ~lc t x =
    Buffer.grow_tail t ;
    let last_idx = Buffer.length ~lc t - 1 in
    let fst = Buffer.get ~lc t last_idx in
    if has_capacity ~lc fst
    then V.push_back ~lc fst x
    else create_child ~lc t last_idx x

  let push_back ~lc t x =
    assert (length ~lc t < capacity ~lc t) ;
    let n = Buffer.length ~lc t - 1 in
    if n < 0
    then push_back_new ~lc t x
    else let tail = Buffer.get ~lc t n in
         if V.is_full ~lc tail
         then push_back_new ~lc t x
         else V.push_back ~lc tail x

  let clean_front ~lc t first =
    if V.is_empty first
    then Buffer.unsafe_pop_front ~lc t

  let pop_front ~lc t =
    let first = Buffer.get ~lc t 0 in
    let v = V.pop_front ~lc first in
    clean_front ~lc t first ;
    v

  let clean_back ~lc t last =
    if V.is_empty last
    then Buffer.unsafe_pop_back ~lc t

  let pop_back ~lc t =
    assert (Buffer.length ~lc t > 0) ;
    let i = Buffer.length ~lc t - 1 in
    let last = Buffer.get ~lc t i in
    let v = V.pop_back ~lc last in
    clean_back ~lc t last ;
    v

  let indexes ~lc t i =
    if i = 0
    then 0, 0
    else let first = Buffer.get ~lc t 0 in
         let first_len = V.length ~lc first in
         if i < first_len
         then 0, i
         else begin
           let i = i - first_len in
           let lcd = lc * V.depth in
           let sector_length = sector_length ~lc in
           let j = 1 + i lsr lcd in
           let i = i land (sector_length - 1) in
           j, i
         end

  let get ~lc t i =
    assert (i >= 0 && i < length ~lc t) ;
    let j, i = indexes ~lc t i in
    let row = Buffer.get ~lc t j in
    V.get ~lc row i

  let set ~lc t i x =
    assert (i >= 0 && i < length ~lc t) ;
    let j, i = indexes ~lc t i in
    let row = Buffer.get ~lc t j in
    V.set ~lc row i x

  let collapse ~lc t j row =
    let len = Buffer.length ~lc t in
    if 2 * j < len
    then begin
      let first = Buffer.get ~lc t 0 in
      let v = ref (V.pop_back ~lc first) in
      for k = 1 to j - 1 do
        let row = Buffer.get ~lc t k in
        v := V.push_front_pop_back ~lc row !v
      done ;
      V.push_front ~lc row !v ;
      clean_front ~lc t first
    end
    else begin
      let len = len - 1 in
      let last = Buffer.get ~lc t len in
      let v = ref (V.pop_front ~lc last) in
      for k = len - 1 downto j + 1 do
        let row = Buffer.get ~lc t k in
        v := V.push_back_pop_front ~lc row !v ;
      done ;
      V.push_back ~lc row !v ;
      clean_back ~lc t last
    end

  let pop_at ~lc t i =
    let j, i = indexes ~lc t i in
    let row = Buffer.get ~lc t j in
    let x = V.pop_at ~lc row i in
    if j = 0
    then clean_front ~lc t row
    else if j = Buffer.length ~lc t - 1
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
    if not (i >= 0 && i <= length ~lc t) then begin
      Printf.printf "insert_at %i / %i\n%!" i (length ~lc t) ;
      failwith "insert_at"
    end ;
    assert (length ~lc t < capacity ~lc t) ;
    if i = 0
    then push_front ~lc t x
    else begin
      let j, i = indexes ~lc t i in
      let len = Buffer.length ~lc t in
      if j = len
      then push_back ~lc t x
      else
        let row = Buffer.get ~lc t j in
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
            let row = Buffer.get ~lc t k in
            v := V.push_back_pop_front ~lc row !v
          done ;
          push_front ~lc t !v
        end
        else begin
          let v =
            if i = V.length ~lc row
            then x
            else begin
              let y = V.pop_back ~lc row in
              V.insert_at ~lc row i x ;
              y
            end
          in
          let v = ref v in
          for k = j + 1 to len - 1 do
            let row = Buffer.get ~lc t k in
            v := V.push_front_pop_back ~lc row !v
          done ;
          push_back ~lc t !v
        end
    end

end
