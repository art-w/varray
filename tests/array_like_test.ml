let make_f () =
  let calls = ref [] in
  let f x =
    calls := x :: !calls ;
    x
  in
  f, calls

let make_fs () =
  let f, fc = make_f () in
  let g, gc = make_f () in
  let check () =
    if !fc <> !gc then failwith "different call order" ;
    !fc = []
  in
  f, g, check

let test name fn =
  try fn () ;
      Printf.printf "OK %s\n%!" name
  with err ->
    Printf.fprintf stderr "ERROR %s\n%s\n%!" name (Printexc.to_string err) ;
    Printexc.print_backtrace stderr ;
    raise err

module Test (V : Varray.S with type 'a elt = 'a and type 'a array = 'a Array.t)
= struct

  let () = Random.self_init ()

  let random_array size =
    let t = V.empty () in
    for _ = 1 to size do
      let x = Random.bits () in
      if Random.bool ()
      then V.push_front t x
      else V.push_back t x
    done ;
    let nb = 1_000 in
    for _ = 1 to nb do
      let x = Random.bits () in
      if Random.bool ()
      then V.push_front t x
      else V.push_back t x
    done ;
    assert (V.length t = size + nb) ;
    let j = Random.int (V.length t + 1) in
    for _ = 1 to nb do
      let x = Random.bits () in
      V.insert_at t j x
    done ;
    assert (V.length t = size + 2 * nb) ;
    for _ = 1 to nb do
      let j = Random.int (V.length t + 1) in
      let x = Random.bits () in
      V.insert_at t j x
    done ;
    assert (V.length t = size + 3 * nb) ;
    for _ = 1 to nb do
      if Random.bool ()
      then ignore (V.pop_front t)
      else ignore (V.pop_back t)
    done ;
    assert (V.length t = size + 2 * nb) ;
    for _ = 1 to 2 * nb do
      let j = Random.int (V.length t) in
      V.delete_at t j
    done ;
    assert (V.length t = size) ;
    t, V.to_array t

  let with_random_array f =
    List.iter
      (fun s -> f (random_array s))
      [ 0 ; 10 ; 100 ; 1000 ]

  let catch f = match f () with
    | _ -> None
    | exception e -> Some e

  let () = test "get" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let n = Array.length arr in
      for i = 0 to n - 1 do
        assert (arr.(i) = V.get t i)
      done ;
      let arr_exn = catch @@ fun () -> arr.(-1) in
      let var_exn = catch @@ fun () -> V.get t (-1) in
      assert (arr_exn = var_exn) ;
      let arr_exn = catch @@ fun () -> arr.(n) in
      let var_exn = catch @@ fun () -> V.get t n in
      assert (arr_exn = var_exn)

  let () = test "set" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let n = Array.length arr in
      for _ = 0 to n - 1 do
        let j = Random.int n in
        let x = Random.bits () in
        arr.(j) <- x ;
        V.set t j x
      done ;
      assert (arr = V.to_array t) ;
      let arr_exn = catch @@ fun () -> arr.(-1) <- 0 in
      let var_exn = catch @@ fun () -> V.set t (-1) 0 in
      assert (arr_exn = var_exn) ;
      let arr_exn = catch @@ fun () -> arr.(n) <- 0 in
      let var_exn = catch @@ fun () -> V.set t n 0 in
      assert (arr_exn = var_exn)

  let () = test "length" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let n = V.length t in
      assert (n = Array.length arr) ;
      V.push_front t 0 ;
      assert (V.length t = n + 1) ;
      V.push_back t 0 ;
      assert (V.length t = n + 2) ;
      let _ = V.pop_front t in
      assert (V.length t = n + 1) ;
      let _ = V.pop_back t in
      assert (V.length t = n) ;
      assert (V.to_array t = arr)

  let () = test "init" @@ fun () ->
    let f, g, check = make_fs () in
    List.iter
      (fun size ->
        let t = V.init size f in
        let arr = Array.init size g in
        assert (V.to_array t = arr) ;
        assert ((size = 0) = check ()))
      [ 0 ; 10 ; 100 ; 1000 ]

  let () = test "empty" @@ fun () ->
    let t = V.empty () in
    assert (V.is_empty t) ;
    assert (V.length t = 0) ;
    assert (V.to_array t = [| |])

  let pr_array t =
    Printf.printf "<%i>  " (Array.length t) ;
    Array.iter (Printf.printf "  %i") t

  let () = test "copy" @@ fun () ->
    with_random_array @@ fun (t0, arr0) ->
      let t1 = V.copy t0 in
      let arr1 = Array.copy arr0 in
      for i = 0 to V.length t1 - 1 do
        V.set t1 i (- i) ;
        Array.set arr1 i (- i) ;
      done ;
      assert (V.to_array t0 = arr0) ;
      assert (V.to_array t1 = arr1)

  let () = test "append" @@ fun () ->
    with_random_array @@ fun (t0, arr0) ->
    with_random_array @@ fun (t1, arr1) ->
      let t01 = V.append t0 t1 in
      let arr01 = Array.append arr0 arr1 in
      assert (V.length t01 = Array.length arr01) ;
      assert (V.to_array t01 = arr01) ;
      for i = 0 to V.length t01 - 1 do
        V.set t01 i (- i)
      done ;
      assert (V.to_array t0 = arr0) ;
      assert (V.to_array t1 = arr1)

  let () = test "concat + sub" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let n = V.length t in
      let rec make_parts i =
        if i >= n
        then []
        else let size = Random.int (n - i + 1) in
             let t = V.sub t i size in
             let arr = Array.sub arr i size in
             (t, arr) :: make_parts (i + size)
      in
      let ts, arrs = List.split @@ make_parts 0 in
      assert (Array.concat arrs = arr) ;
      assert (V.to_array (V.concat ts) = arr) ;
      let ts, arrs = List.rev ts, List.rev arrs in
      assert (V.to_array (V.concat ts) = Array.concat arrs)

  let () = test "fill" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let len = V.length t in
      if len > 0
      then begin
        for step = 0 to 100 do
          let i = Random.int len in
          let s = Random.int (len - i) in
          V.fill t i s step ;
          Array.fill arr i s step ;
          assert (V.to_array t = arr)
        done
      end ;

      let et = catch @@ fun () -> V.fill t 0 (-1) 42 in
      let at = catch @@ fun () -> Array.fill arr 0 (-1) 42 in
      assert (et = Some (Invalid_argument "Varray.fill")) ;
      assert (at = Some (Invalid_argument "Array.fill")) ;

      let et = catch @@ fun () -> V.fill t (len + 1) 0 42 in
      let at = catch @@ fun () -> Array.fill arr (len + 1) 0 42 in
      assert (et = Some (Invalid_argument "Varray.fill")) ;
      assert (at = Some (Invalid_argument "Array.fill")) ;

      let et = catch @@ fun () -> V.fill t 1 len 42 in
      let at = catch @@ fun () -> Array.fill arr 1 len 42 in
      assert (et = Some (Invalid_argument "Varray.fill")) ;
      assert (at = Some (Invalid_argument "Array.fill")) ;

      assert (V.to_array t = arr)

  let () = test "blit" @@ fun () ->
    with_random_array @@ fun (t0, arr0) ->
    with_random_array @@ fun (t1, arr1) ->
      let size = Random.int (1 + min (V.length t0) (V.length t1)) in
      let src_pos = Random.int (1 + V.length t0 - size) in
      let dst_pos = Random.int (1 + V.length t1 - size) in
      V.blit t0 src_pos t1 dst_pos size ;
      Array.blit arr0 src_pos arr1 dst_pos size ;
      assert (V.to_array t1 = arr1) ;

      let et = catch @@ fun () -> V.blit t0 (-1) t1 0 0 in
      let at = catch @@ fun () -> Array.blit arr0 (-1) arr1 0 0 in
      assert (et = Some (Invalid_argument "Varray.blit")) ;
      assert (at = Some (Invalid_argument "Array.blit")) ;

      let et = catch @@ fun () -> V.blit t0 0 t1 (-1) 0 in
      let at = catch @@ fun () -> Array.blit arr0 0 arr1 (-1) 0 in
      assert (et = Some (Invalid_argument "Varray.blit")) ;
      assert (at = Some (Invalid_argument "Array.blit")) ;

      let et = catch @@ fun () -> V.blit t0 0 t1 0 (-1) in
      let at = catch @@ fun () -> Array.blit arr0 0 arr1 0 (-1) in
      assert (et = Some (Invalid_argument "Varray.blit")) ;
      assert (at = Some (Invalid_argument "Array.blit")) ;

      let len1, len0 = V.length t1, V.length t0 in
      let et = catch @@ fun () -> V.blit t0 0 t1 len1 len0 in
      let at = catch @@ fun () -> Array.blit arr0 0 arr1 len1 len0 in
      let fail = not (V.is_empty t0) in
      assert (fail = (at = Some (Invalid_argument "Array.blit"))) ;
      assert (fail = (et = Some (Invalid_argument "Varray.blit"))) ;

      assert (V.to_array t0 = arr0) ;
      assert (V.to_array t1 = arr1)

  let () = test "to_list + of_list" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let xs = V.to_list t in
      let ys = Array.to_list arr in
      assert (xs = ys) ;
      let t' = V.of_list xs in
      let arr' = Array.of_list ys in
      assert (arr = arr') ;
      assert (t <> t') ;
      assert (V.to_array t' = arr') ;
      assert (V.for_all2 ( == ) t t')

  let () = test "iter" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let f, g, check = make_fs () in
      V.iter (fun x -> ignore (f x)) t ;
      Array.iter (fun x -> ignore (g x)) arr ;
      assert (V.is_empty t = check ())

  let () = test "iteri" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let f, g, check = make_fs () in
      V.iteri (fun i x -> ignore (f (i, x))) t ;
      Array.iteri (fun i x -> ignore (g (i, x))) arr ;
      assert (V.is_empty t = check ())

  let () = test "map" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let f, g, check = make_fs () in
      let t' = V.map f t in
      let arr' = Array.map g arr in
      assert (V.to_array t' = arr') ;
      assert (V.is_empty t = check ())

  let () = test "mapi" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let f, g, check = make_fs () in
      let t' = V.mapi (fun i x -> f (i, x)) t in
      let arr' = Array.mapi (fun i x -> g (i, x)) arr in
      assert (V.to_array t' = arr') ;
      assert (V.is_empty t = check ())

  let () = test "fold_left" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let f, g, check = make_fs () in
      let f acc x = ignore (f (acc, x)) ; x in
      let x = V.fold_left f min_int t in
      let g acc x = ignore (g (acc, x)) ; x in
      let y = Array.fold_left g min_int arr in
      assert (x = y) ;
      assert (x = if V.is_empty t then min_int else V.get t (V.length t - 1)) ;
      assert (V.is_empty t = check ())

  let () = test "fold_right" @@ fun () ->
    with_random_array @@ fun (t, arr) ->
      let f, g, check = make_fs () in
      let f x acc = ignore (f (acc, x)) ; x in
      let x = V.fold_right f t min_int in
      let g x acc = ignore (g (acc, x)) ; x in
      let y = Array.fold_right g arr min_int in
      assert (x = y) ;
      assert (x = if V.is_empty t then min_int else V.get t 0) ;
      assert (V.is_empty t = check ())

  let () = test "fold_left_map" @@ fun () ->
    with_random_array @@ fun (t, _arr) ->
      (* requires ocaml 4.13.0 *)
      (* let f, g, check = make_fs () in *)
      let f acc x = (* ignore (f (acc, x)) ; *) x, acc in
      let x, t' = V.fold_left_map f min_int t in
      assert (x = if V.is_empty t then min_int else V.get t (V.length t - 1)) ;
      (*
      let g acc x = ignore (g (acc, x)) ; x, acc in
      let y, arr' = Array.fold_left_map g min_int arr in
      assert (x = y) ;
      assert (V.is_empty t = check ()) ;
      assert (V.to_array t' = arr') ;
      assert (V.is_empty t = check ()) ;
      *)
      V.push_front t min_int ;
      let _ = V.pop_back t in
      assert (V.to_array t = V.to_array t')

  let () = test "index_out_of_bounds" @@ fun () ->
    with_random_array @@ fun (t, _arr) ->
      let is_invalid fn =
        match fn () with
        | exception Invalid_argument _ -> ()
        | _ -> failwith "expected Invalid_argument exception"
      in
      is_invalid (fun () -> V.make (-1) 42) ;
      is_invalid (fun () -> V.get t (-1)) ;
      is_invalid (fun () -> V.get t (V.length t)) ;
      is_invalid (fun () -> V.set t (-1) 42) ;
      is_invalid (fun () -> V.set t (V.length t) 42) ;
      is_invalid (fun () -> V.pop_at t (-1)) ;
      is_invalid (fun () -> V.pop_at t (V.length t)) ;
      is_invalid (fun () -> V.delete_at t (-1)) ;
      is_invalid (fun () -> V.delete_at t (V.length t)) ;
      is_invalid (fun () -> V.insert_at t (-1) 42) ;
      is_invalid (fun () -> V.insert_at t (V.length t + 1) 42) ;
      is_invalid (fun () -> V.blit t (-1) t 0 1) ;
      is_invalid (fun () -> V.blit t 0 t (-1) 1) ;
      is_invalid (fun () -> V.blit t 0 t 0 (-1)) ;
      is_invalid (fun () -> V.blit t 0 t 0 (V.length t + 1)) ;
      is_invalid (fun () -> V.blit t 1 t 0 (V.length t)) ;
      is_invalid (fun () -> V.blit t 0 t 1 (V.length t)) ;
      is_invalid (fun () -> V.sub t 1 (V.length t)) ;
      is_invalid (fun () -> V.sub t (-1) 1) ;
      is_invalid (fun () -> V.sub t (V.length t) 1) ;
      is_invalid (fun () -> V.sub t (V.length t + 1) 0) ;
      assert (V.length (V.sub t 0 0) = 0) ;
      assert (V.length (V.sub t (V.length t) 0) = 0) ;
      is_invalid (fun () -> V.fill t 1 (V.length t) 42) ;
      is_invalid (fun () -> V.fill t (-1) 1 42) ;
      is_invalid (fun () -> V.fill t (V.length t) 1 42) ;
      is_invalid (fun () -> V.fill t (V.length t + 1) 0 42)
end

let header i = Printf.printf "------------ V%i ------------\n%!" i

open Varray

let () = header 1
module V1 = Circular
module T1 = Test (V1)

let () = header 2
module V2 = Root (V1)
module T2 = Test (V2)

let () = header 3
module V3 = Root (V2)
module T3 = Test (V3)

let () = header 4
module V4 = Root (V3)
module T4 = Test (V4)

let () = header 5
module V5 = Root (V4)
module T5 = Test (V5)

let () = header 6
module V6 = Root (V5)
module T6 = Test (V6)

let () = header 7
module V7 = Root (V6)
module T7 = Test (V7)
