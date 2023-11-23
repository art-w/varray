module Make (Arg : Varray_sig.VARRAY)
: Varray_sig.S with type 'a elt = 'a Arg.elt and type 'a array = 'a Arg.array
= struct

  include Arg

  let sub t pos len =
    if pos < 0 || len < 0 || pos + len > length t
    then invalid_arg "Varray.sub" ;
    init len (fun i -> get t (pos + i))

  let copy t = sub t 0 (length t)

  let fill t i n x =
    if i < 0 || n < 0 || i + n >= length t
    then invalid_arg "Varray.fill" ;
    for j = i to i + n - 1 do
      set t j x
    done

  let blit src src_pos dst dst_pos len =
    if src = dst && src_pos < dst_pos
    then for j = len - 1 downto 0 do
           set dst (j + dst_pos) (get src (j + src_pos))
         done
    else for j = 0 to len - 1 do
           set dst (j + dst_pos) (get src (j + src_pos))
         done

  let append a b =
    match length a, length b with
    | 0, _ -> copy b
    | _, 0 -> copy a
    | a_len, b_len ->
        let x = get a 0 in
        let t = make (a_len + b_len) x in
        blit a 1 t 1 (a_len - 1) ;
        blit b 0 t a_len b_len ;
        t

  let rec concat = function
    | [] -> empty ()
    | t :: ts when is_empty t -> concat ts
    | (t :: _) as lst ->
        let len =
          List.fold_left (fun acc t -> acc + length t) 0 lst
        in
        let x = get t 0 in
        let result = make len x in
        let _ =
          List.fold_left
            (fun acc t ->
              let n = length t in
              blit t 0 result acc n ;
              acc + n)
            0
            lst
        in
        result

  let iter f t =
    protect t @@ fun () ->
    for i = 0 to length t - 1 do
      f (get t i)
    done

  let iteri f t =
    protect t @@ fun () ->
    for i = 0 to length t - 1 do
      f i (get t i)
    done

  let map f t = match length t with
    | 0 -> empty ()
    | n ->
        let x = f (get t 0) in
        let r = make n x in
        for i = 1 to n - 1 do
          set r i (f (get t i))
        done ;
        r

  let map f t = protect t (fun () -> map f t)

  let mapi f t = match length t with
    | 0 -> empty ()
    | n ->
        let x = f 0 (get t 0) in
        let r = make n x in
        for i = 1 to n - 1 do
          set r i (f i (get t i))
        done ;
        r

  let mapi f t = protect t (fun () -> mapi f t)

  let fold_left f z t =
    let acc = ref z in
    for i = 0 to length t - 1 do
      acc := f !acc (get t i)
    done ;
    !acc

  let fold_left f z t = protect t (fun () -> fold_left f z t)

  let fold_right f t z =
    let acc = ref z in
    for i = length t - 1 downto 0 do
      acc := f (get t i) !acc
    done ;
    !acc

  let fold_right f t z = protect t (fun () -> fold_right f t z)

  let fold_left_map f z t = match length t with
    | 0 -> z, empty ()
    | n ->
        let z, x = f z (get t 0) in
        let r = make n x in
        let acc = ref z in
        for i = 1 to n - 1 do
          let z, x = f !acc (get t i) in
          acc := z ;
          set r i x
        done ;
        !acc, r

  let fold_left_map f z t = protect t (fun () -> fold_left_map f z t)

  let iter2 f xs ys =
    let n, ys_len = length xs, length ys in
    if n <> ys_len then invalid_arg "Varray.iter2" ;
    for i = 0 to n - 1 do
      f (get xs i) (get ys i)
    done

  let iter2 f xs ys =
    protect xs (fun () -> protect ys (fun () -> iter2 f xs ys))

  let map2 f xs ys =
    let n, ys_len = length xs, length ys in
    if n <> ys_len
    then invalid_arg "Varray.map2"
    else if n = 0
    then empty ()
    else begin
      let x = f (get xs 0) (get ys 0) in
      let t = make n x in
      for i = 1 to n - 1 do
        let x = f (get xs i) (get ys i) in
        set t i x
      done ;
      t
    end

  let map2 f xs ys =
    protect xs (fun () -> protect ys (fun () -> map2 f xs ys))

  exception Abort

  let for_all f t =
    try iter (fun x -> if not (f x) then raise Abort) t ;
        true
    with Abort -> false

  let for_all2 f xs ys =
    try iter2 (fun x y -> if not (f x y) then raise Abort) xs ys ;
        true
    with Abort -> false

  let exists f t =
    try iter (fun x -> if f x then raise Abort) t ;
        false
    with Abort -> true

  let exists2 f xs ys =
    try iter2 (fun x y -> if f x y then raise Abort) xs ys ;
        false
    with Abort -> true

  let mem x t = exists (( = ) x) t
  let memq x t = exists (( == ) x) t

  let find_opt (type a) f t =
    let exception Found of a elt in
    try iter (fun x -> if f x then raise (Found x)) t ;
        None
    with Found x -> Some x

  let find_map (type a) f t =
    let exception Found of a in
    let search x = match f x with
      | None -> ()
      | Some y -> raise (Found y)
    in
    try iter search t ;
        None
    with Found x -> Some x

  let to_std_array t = Stdlib.Array.init (length t) (get t)

  let sort cmp t =
    let arr = to_std_array t in
    Stdlib.Array.sort cmp arr ;
    Stdlib.Array.iteri (set t) arr

  let stable_sort cmp t =
    let arr = to_std_array t in
    Stdlib.Array.stable_sort cmp arr ;
    Stdlib.Array.iteri (set t) arr

  let fast_sort cmp t =
    let arr = to_std_array t in
    Stdlib.Array.fast_sort cmp arr ;
    Stdlib.Array.iteri (set t) arr

  let of_array arr =
    init (Tier.Array.length arr) (Tier.Array.get arr)

  let to_array t =
    let n = length t in
    let arr = Arg.Tier.Array.create n in
    for i = 0 to n - 1 do
      Tier.Array.set arr i (get t i)
    done ;
    arr

  let to_list t = fold_right (fun x xs -> x :: xs) t []

  let of_list = function
    | [] -> empty ()
    | (x :: _) as lst ->
        let len = List.length lst in
        let t = make len x in
        List.iteri (fun i x -> set t i x) lst ;
        t

  let blit src src_pos dst dst_pos len =
    if src_pos < 0 || dst_pos < 0 || len < 0
    || src_pos + len > length src
    || dst_pos + len > length dst
    then invalid_arg "Varray.blit" ;
    blit src src_pos dst dst_pos len

  let pop_front t =
    if is_empty t then raise Not_found ;
    pop_front t

  let pop_back t =
    if is_empty t then raise Not_found ;
    pop_back t

  let pop_at t i =
    if i < 0 || i >= length t
    then invalid_arg "Varray.pop_at: index out of bounds" ;
    pop_at t i

  let delete_at t i =
    if i < 0 || i >= length t
    then invalid_arg "Varray.delete_at: index out of bounds" ;
    delete_at t i

  let insert_at t i =
    if i < 0 || i > length t
    then invalid_arg "Varray.insert_at: index out of bounds" ;
    insert_at t i

  let make n x =
    if n < 0 then invalid_arg "Varray.make" ;
    make n x

  let init n f =
    if n < 0 then invalid_arg "Varray.init" ;
    init n f

end
