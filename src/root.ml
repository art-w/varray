let rec pow x = function
  | 0 -> 1
  | 1 -> x
  | 2 -> x * x
  | 3 -> x * x * x
  | n when n mod 2 = 1 -> x * pow x (n - 1)
  | n -> pow (x * x) (n / 2)

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

  let root_capacity t = Buffer.capacity t - 1

  let sector_length t = pow (root_capacity t) V.depth

  let capacity t = root_capacity t * sector_length t

  let length t = match Buffer.length t with
    | 0 -> 0
    | 1 -> V.length (Buffer.get t 0)
    | n ->
        let first = Buffer.get t 0 in
        let last = Buffer.get t (n - 1) in
        let sector_length = sector_length t in
        V.length first + V.length last + sector_length * (n - 2)

  let is_full t = length t = capacity t

  let create ~capacity =
    Buffer.create ~capacity:(capacity + 1)

  let make ~capacity n x =
    assert (capacity > 0) ;
    let sector_length = pow capacity V.depth in
    let nb_full_parts = n / sector_length in
    let remaining = n - (sector_length * nb_full_parts) in
    let nb = if remaining > 0 then 1 + nb_full_parts else nb_full_parts in
    let rows = Buffer.make ~capacity:(capacity + 1) nb V.empty in
    for i = 0 to nb_full_parts - 1 do
      let row = V.make ~capacity sector_length x in
      Buffer.set rows i row
    done ;
    if remaining > 0
    then begin
      let row = V.make ~capacity remaining x in
      Buffer.set rows nb_full_parts row ;
    end ;
    rows

  let init ~capacity ~offset n f =
    assert (capacity > 0) ;
    let sector_length = pow capacity V.depth in
    let nb_full_parts = n / sector_length in
    let full = sector_length * nb_full_parts in
    let remaining = n - full in
    let nb = if remaining > 0 then 1 + nb_full_parts else nb_full_parts in
    let rows = Buffer.make ~capacity:(capacity + 1) nb V.empty in
    for i = 0 to nb_full_parts - 1 do
      let offset = offset + i * sector_length in
      let row = V.init ~capacity ~offset sector_length f in
      Buffer.set rows i row
    done ;
    if remaining > 0
    then begin
      let row = V.init ~capacity ~offset:(offset + full) remaining f in
      Buffer.set rows nb_full_parts row ;
    end ;
    rows

  let has_capacity v = V.root_capacity v > 0

  let create_child t i x =
    let row = V.make ~capacity:(root_capacity t) 1 x in
    Buffer.set t i row

  let push_front_new t x =
    Buffer.grow_head t ;
    let fst = Buffer.get t 0 in
    if has_capacity fst
    then V.push_front fst x
    else create_child t 0 x

  let push_front t x =
    assert (length t < capacity t) ;
    if Buffer.is_empty t
    then push_front_new t x
    else let head = Buffer.get t 0 in
         if V.is_full head
         then push_front_new t x
         else V.push_front head x

  let push_back_new t x =
    Buffer.grow_tail t ;
    let last_idx = Buffer.length t - 1 in
    let fst = Buffer.get t last_idx in
    if has_capacity fst
    then V.push_back fst x
    else create_child t last_idx x

  let push_back t x =
    assert (length t < capacity t) ;
    let n = Buffer.length t - 1 in
    if n < 0
    then push_back_new t x
    else let tail = Buffer.get t n in
         if V.is_full tail
         then push_back_new t x
         else V.push_back tail x

  let clean_front t first =
    if V.is_empty first then Buffer.unsafe_pop_front t

  let pop_front t =
    let first = Buffer.get t 0 in
    let v = V.pop_front first in
    clean_front t first ;
    v

  let clean_back t last =
    if V.is_empty last
    then Buffer.unsafe_pop_back t

  let pop_back t =
    assert (Buffer.length t > 0) ;
    let i = Buffer.length t - 1 in
    let last = Buffer.get t i in
    let v = V.pop_back last in
    clean_back t last ;
    v

  let indexes t i =
    if i = 0
    then 0, 0
    else let first = Buffer.get t 0 in
         let first_len = V.length first in
         if i < first_len
         then 0, i
         else begin
           let i = i - first_len in
           let sector_length = sector_length t in
           let j = 1 + i / sector_length in
           let i = i - (j - 1) * sector_length in
           assert (i >= 0 && i < sector_length) ;
           assert (j >= 0 && j <= Buffer.length t) ;
           j, i
         end

  let get t i =
    assert (i >= 0 && i < length t) ;
    let j, i = indexes t i in
    let row = Buffer.get t j in
    V.get row i

  let set t i x =
    assert (i >= 0 && i < length t) ;
    let j, i = indexes t i in
    let row = Buffer.get t j in
    V.set row i x

  let collapse t j row =
    let len = Buffer.length t in
    if 2 * j < len
    then begin
      let first = Buffer.get t 0 in
      let v = ref (V.pop_back first) in
      for k = 1 to j - 1 do
        let row = Buffer.get t k in
        v := V.push_front_pop_back row !v
      done ;
      V.push_front row !v ;
      clean_front t first
    end
    else begin
      let len = len - 1 in
      let last = Buffer.get t len in
      let v = ref (V.pop_front last) in
      for k = len - 1 downto j + 1 do
        let row = Buffer.get t k in
        v := V.push_back_pop_front row !v ;
      done ;
      V.push_back row !v ;
      clean_back t last
    end

  let pop_at t i =
    let j, i = indexes t i in
    let row = Buffer.get t j in
    let x = V.pop_at row i in
    if j = 0
    then clean_front t row
    else if j = Buffer.length t - 1
    then clean_back t row
    else collapse t j row ;
    x

  let push_front_pop_back t x =
    let y = pop_back t in
    push_front t x ;
    y

  let push_back_pop_front t x =
    let y = pop_front t in
    push_back t x ;
    y

  let insert_at t i x =
    assert (i >= 0 && i <= length t) ;
    assert (length t < capacity t) ;
    if i = 0
    then push_front t x
    else begin
      let j, i = indexes t i in
      let len = Buffer.length t in
      if j = len
      then push_back t x
      else
        let row = Buffer.get t j in
        if 2 * j < len
        then begin
          let v =
            if i = 0
            then x
            else begin
              let y = V.pop_front row in
              V.insert_at row (i - 1) x ;
              y
            end
          in
          let v = ref v in
          for k = j - 1 downto 0 do
            let row = Buffer.get t k in
            v := V.push_back_pop_front row !v
          done ;
          push_front t !v
        end
        else begin
          let v =
            if i = V.length row
            then x
            else begin
              let y = V.pop_back row in
              V.insert_at row i x ;
              y
            end
          in
          let v = ref v in
          for k = j + 1 to len - 1 do
            let row = Buffer.get t k in
            v := V.push_front_pop_back row !v
          done ;
          push_back t !v
        end
    end

end
