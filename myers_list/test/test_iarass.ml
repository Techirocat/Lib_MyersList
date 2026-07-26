open Myers_list
open Iarass

let test_empty () =
	let l = empty in 
	Alcotest.(check bool) "Test if a Empty List is created" true (is_empty l)


let test_is_empty () =
	let le = empty in 
	let l = cons 10 empty in 
	Alcotest.(check bool) "Test if is empty" true (is_empty le); 
	Alcotest.(check bool) "Test if is empty" false (is_empty l)


let test_cons () =
	let l = empty in 
	Alcotest.(check int) "Test cons" 0 (length l);

	let li = cons 10 l in 
	Alcotest.(check int) "Test cons" 1 (length li); 
	Alcotest.(check int) "Test cons" 10 (hd li);

	let lis = cons 20 li in 
	Alcotest.(check int) "Test cons" 2 (length lis); 
	Alcotest.(check int) "Test cons" 20 (hd lis);
	Alcotest.(check int) "Test cons" 10 (last lis)


let test_return () = 
	let l = return 10 in 
	Alcotest.(check int) "Test return" 1 (length l); 
	Alcotest.(check int) "Test return" 10 (hd l)


let test_map () =
	let f = (fun x -> x * 2) in 
	let l = of_list [1; 2; 3; 4; 5] in 
	let expected = [2; 4; 6; 8 ; 10] in 
	Alcotest.(check (list int)) "Test map" expected (to_list (map ~f l));
	Alcotest.(check (list int)) "Test map" [] (to_list (map ~f empty))


let test_mapi () = 
	let f = (fun i x -> x * i) in 
	let l = of_list [1; 2; 3; 4; 5] in 
	let expected = [0; 2; 6; 12 ; 20] in 
	Alcotest.(check (list int)) "Test mapi" expected (to_list (mapi ~f l));
	Alcotest.(check (list int)) "Test mapi" [] (to_list(mapi ~f empty))


let test_hd () = 
	let l = of_list [99; 2; 3] in
  	Alcotest.(check int) "Test hd" 99 (hd l);
	Alcotest.check_raises "Test hd invalid" (Invalid_argument "Empty List") (fun () -> ignore (hd empty))


let test_last () = 
	let l = of_list [99; 2; 3] in
  	Alcotest.(check int) "Test last" 3 (last l);
	Alcotest.check_raises "Test last invalid" (Invalid_argument "Empty List") (fun () -> ignore (last empty))


let test_tl () =
	let l = of_list [1; 2; 3] in
  	let tail = tl l in
  	Alcotest.(check (list int)) "Test tl" [2; 3] (to_list tail);
	Alcotest.(check (list int)) "Test tl" [] (to_list (tl (return 10)));
	Alcotest.check_raises "Test tl invalid" (Invalid_argument "Empty List") (fun () -> ignore (tl empty))


let test_front () = 
	let l = of_list [1; 2; 3] in 
	let (v, lst) =  Option.get (front l) in 
  	Alcotest.(check bool) "Test front" true (front empty = None);
  	Alcotest.(check int) "Test front" 1 v;
  	Alcotest.(check int) "Test front" 2 (hd lst)
  	

let test_front_exn () = 
	let l = of_list [1; 2; 3] in 
	let (v, lst) = front_exn l in 
    Alcotest.(check int) "Test front exn" 1 v;
	Alcotest.(check int) "Test front exn" 2 (hd lst);
    Alcotest.check_raises "Test front exn" (Invalid_argument "Empty List") (fun () -> ignore (front_exn empty))

let test_length () = 
	let l = of_list [1; 2; 3; 4; 5] in
  	Alcotest.(check int) "Test length" 5 (length l);
  	Alcotest.(check int) "Test length" 0 (length empty)

let test_get () = 
	let l = of_list [0; 1; 2; 3; 4; 5] in 
  	Alcotest.(check (option int)) "Get 4" (Some 5) (get l 5);
	Alcotest.(check (option int)) "Get 4" (Some 4) (get l 4);
	Alcotest.(check (option int)) "Get 4" (Some 3) (get l 3);
	Alcotest.(check (option int)) "Get 4" (Some 2) (get l 2);
	Alcotest.(check (option int)) "Get 4" (Some 1) (get l 1);
  	Alcotest.(check (option int)) "Get 0" (Some 0) (get l 0);
  	Alcotest.(check (option int)) "Get invalid index" None (get l (-1));
  	Alcotest.(check (option int)) "Get invalid index" None (get l 13);
	Alcotest.(check (option int)) "Get in Empty List" None (get empty 1)


let test_get_exn () = 
	let l = of_list [10; 20; 30] in
    Alcotest.(check int) "Test get_exn" 20 (get_exn l 1);
    Alcotest.check_raises "Test get exn invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (get_exn l 5));
	Alcotest.check_raises "Test get exn invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (get_exn l (-1)));
	Alcotest.check_raises "Test get exn invalid" (Invalid_argument "Empty List") (fun () -> ignore (get_exn empty 5))


let test_set () = 
	let l = of_list [10; 20; 30; 40] in
  	Alcotest.(check (list int)) "Test set" [99; 20; 30; 40] (to_list (set l 0 99));
	Alcotest.(check (list int)) "Test set" [10; 99; 30; 40] (to_list (set l 1 99));
	Alcotest.(check (list int)) "Test set" [10; 20; 99; 40] (to_list (set l 2 99));
	Alcotest.(check (list int)) "Test set" [10; 20; 30; 99] (to_list (set l 3 99));

	Alcotest.check_raises "Test set invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (set l (-1) 100));
	Alcotest.check_raises "Test set invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (set l 20 100));
	Alcotest.check_raises "Test set invalid" (Invalid_argument "Empty List") (fun () -> ignore (set empty 1 99))


let test_remove () = 
	let l = of_list [10; 20; 30] in
	let li = of_list [10] in 
	let lis = empty in 


	Alcotest.(check (list int)) "Test remove" [20; 30] (to_list (remove l 0));
	Alcotest.(check (list int)) "Test remove" [10; 30] (to_list (remove l 1));
 	Alcotest.(check (list int)) "Test remove" [10; 20] (to_list (remove l 2));

	Alcotest.check_raises "Test remove invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (remove li 10));
	Alcotest.check_raises "Test remove invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (remove li (-1)));
	Alcotest.(check bool) "Test remove" true (is_empty (remove li 0));

	Alcotest.check_raises "Test remove invalid" (Invalid_argument "Empty List") (fun () -> ignore (remove lis 0))


let test_get_and_remove_exn () = 
	let l = of_list [10; 20; 30] in
	let li = of_list [10; 20; 30; 40; 50] in 

  	let (v, lst_new) = get_and_remove_exn l 1 in
  	Alcotest.(check int) "Test get and remove" 20 v;
  	Alcotest.(check (list int)) "Test get and remove" [10; 30] (to_list lst_new);

  	let (v1, lst_new1) = get_and_remove_exn li 2 in
  	Alcotest.(check int) "Test get and remove" 30 v1;
  	Alcotest.(check (list int)) "Test get and remove" [10; 20; 40; 50] (to_list lst_new1);

	Alcotest.check_raises "Test get and remove invalid" (Invalid_argument "Empty List") (fun () -> ignore (get_and_remove_exn empty 1));
	Alcotest.check_raises "Test get and remove invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (get_and_remove_exn l 10));
	Alcotest.check_raises "Test get and remove invalid" (Invalid_argument "Invalid Index") (fun () -> ignore (get_and_remove_exn l (-1)))


let test_append () = 
	let l1 = of_list [1; 2] in
  	let l2 = of_list [3; 4] in
  	Alcotest.(check (list int)) "Test append" [1; 2; 3; 4] (to_list (append l1 l2))


let test_filter () = 
	let lst = of_list [1; 2; 3; 4; 5; 6] in
  	let fil = filter ~f:(fun x -> x mod 2 = 0) lst in
  	Alcotest.(check (list int)) "Test filter" [2; 4; 6] (to_list fil)


let test_filter_map () =
	let lst = of_list [1; 2; 3; 4; 5] in
    let f x = if x mod 2 = 0 then Some (x * 10) else None in
    Alcotest.(check (list int)) "Test filter_map" [20; 40] (to_list (filter_map ~f lst))


let test_flat_map () = 
	let l = of_list [1; 2; 3] in
    let f x = of_list [x; x * 10] in 
    let expected = [1; 10; 2; 20; 3; 30] in 
    
    Alcotest.(check (list int)) "Test flat_map" expected (to_list (flat_map f l));    
    Alcotest.(check (list int)) "Test flat_map" [] (to_list (flat_map f empty))	

let test_flatten () =
	let l = of_list [of_list [1; 2]; of_list [3; 4]] in
  Alcotest.(check (list int)) "Test flatten" [1; 2; 3; 4] (to_list (flatten l)) 


let test_app () = 
	let f1 = (fun x -> x + 1) in 
  	let f2 = (fun x -> x * 2) in 
  	let l = of_list [1; 2; 3] in 
  	let funs = of_list [f1; f2] in 
  	let expected = [2; 3; 4; 2; 4; 6] in 
  	Alcotest.(check (list int)) "Test app" expected (to_list (app funs l))


let test_take () = 
	let l = of_list [1; 2; 3; 4; 5] in 
	let expected = [1; 2; 3] in 

  Alcotest.(check (list int)) "Test take" expected (to_list (take 3 l));
	Alcotest.(check bool) "Test take" true (is_empty(take 1 empty));
	Alcotest.(check bool) "Test take" true (is_empty(take (-10) l))


	
let test_take_while () =
	let l = of_list [1; 2; 3; 4; 1] in
	let f = (fun x -> x < 3) in 
	let f1 = (fun x -> x > 100) in 

  Alcotest.(check (list int)) "Test take while" [1; 2] (to_list (take_while ~f l));
	Alcotest.(check (list int)) "Test take while" [] (to_list (take_while ~f:f1 l))




let test_drop () = 
	let l = of_list [1; 2; 3; 4; 5] in
  Alcotest.(check (list int)) "Test drop" [3; 4; 5] (to_list (drop 2 l));
	Alcotest.check_raises "Test drop invalid" (Invalid_argument "Invalid Argument") (fun () -> ignore (drop (-1) l));
	Alcotest.check_raises "Test drop invalid" (Invalid_argument "Invalid Argument") (fun () -> ignore (drop 10 l));
	Alcotest.(check (list int)) "Test drop" [] (to_list (drop 5 l))



let test_drop_while () =
	let l = of_list [1; 2; 3; 4; 1] in
	let f = (fun x -> x < 3) in 
	let f1 = (fun x -> x > 100) in 

  Alcotest.(check (list int)) "Test drop while" [3; 4; 1] (to_list (drop_while ~f l));
	Alcotest.(check (list int)) "Test drop while" [1; 2; 3; 4; 1] (to_list (drop_while ~f:f1 l))



let test_take_drop () =
	let l = of_list [1; 2; 3; 4; 5] in
  	let (t, d) = take_drop 2 l in
  	Alcotest.(check (list int)) "Test take drop (take)" [1; 2] (to_list t);
  	Alcotest.(check (list int)) "Test take drop (drop)" [3; 4; 5] (to_list d) 


let test_iter () = 
	let l = of_list [1; 2; 3] in
  	let sum = ref 0 in
  	iter ~f:(fun x -> sum := !sum + x) l;
  	Alcotest.(check int) "Test iter (soma com refs)" 6 !sum


let test_iteri () = 
	let l = of_list [10; 20] in
  	let sum = ref 0 in
  	iteri ~f:(fun i x -> sum := !sum + (i * x)) l;
  	Alcotest.(check int) "Test iteri" 20 !sum


let test_fold () = 
	let l = of_list [1; 2; 3; 4] in
  	let sum = fold ~f:(fun acc x -> acc + x) ~x:0 l in
  	Alcotest.(check int) "Test fold" 10 sum


let test_fold_rev () = 
	let l = of_list [1; 2; 3] in
  	let res = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] l in
  	Alcotest.(check (list int)) "Test fold_rev" [1; 2; 3] res


let test_rev_map () = 
	let l = of_list [1; 2; 3] in
  Alcotest.(check (list int)) "Test rev_map" [30; 20; 10] (to_list (rev_map ~f:(fun x -> x * 10) l))


let test_rev () = 
	let l = of_list [0; 1; 2; 3; 4; 5] in
  	let reversed = rev l in
 	Alcotest.(check (list int)) "Test rev" [5; 4; 3; 2; 1; 0] (to_list reversed);
  	Alcotest.(check bool) "Test rev" true (is_empty (rev empty))


let test_equal () = 
	let l1 = of_list [1; 2; 3] in
  	let l2 = of_list [1; 2; 3] in
  	let l3 = of_list [10; 20; 30] in
	let l4 = of_list [1; 2; 99] in
	let l5 = of_list [10] in 

  Alcotest.(check bool) "Test equals" true (equal ~eq:(=) l1 l2);
  Alcotest.(check bool) "Test equals" false (equal ~eq:(=) l1 l3);
	Alcotest.(check bool) "Test equals" false (equal ~eq:(=) l1 l4);
	Alcotest.(check bool) "Test equals" false (equal ~eq:(=) l1 l5);
	Alcotest.(check bool) "Test equals" true (equal ~eq:(=) l5 l5)



let test_compare () = 
	let l1 = of_list [1; 2; 3] in
    let l2 = of_list [10; 20; 30] in
    let l3 = of_list [1; 2; 3] in
	let l4 = of_list [1; 2] in
	let cmp = Stdlib.compare in
  Alcotest.(check int) "Test compare" 0 (compare ~cmp l1 l3);
  Alcotest.(check int) "Test compare" (-1) (compare ~cmp l1 l2);
    Alcotest.(check int) "Test compare" 1 (compare ~cmp l2 l1);

	Alcotest.(check int) "Test compare" (-1) (compare ~cmp l4 l1);
	Alcotest.(check int) "Test compare" 1 (compare ~cmp l1 l4)



let test_make () = 
	let l = make 3 10 in
	let li = make 0 10 in 
    Alcotest.(check (list int)) "Test make" [10; 10; 10] (to_list l);
	    Alcotest.(check (list int)) "Test make" [] (to_list li)



let test_repeat () = 
	let w = of_list [1; 2; 3] in 
	let l = repeat 3 w in
	let li = repeat 0 w in 
    Alcotest.(check (list int)) "Test repeat" [1; 2; 3; 1; 2; 3; 1; 2; 3] (to_list l);
	Alcotest.(check (list int)) "Test repeat" [] (to_list li);
	Alcotest.check_raises "Test repeat invalid" (Invalid_argument "Invalid Argument") (fun () -> ignore (repeat (-10) w));
	Alcotest.(check bool) "Test repeat" true (is_empty (repeat 10 empty))

	

let test_range () = 
	let lst = range 2 5 in
	let l1 = range 1 1 in
	let l2 = range 5 2 in

 	Alcotest.(check (list int)) "Test Range" [2; 3; 4; 5] (to_list lst);
	Alcotest.(check (list int)) "Test Range" [1] (to_list l1);
 	Alcotest.(check (list int)) "Test Range" [5; 4; 3; 2] (to_list l2)



let test_add_list () = 
	let l = add_list empty [1; 2; 3] in
  	Alcotest.(check (list int)) "Test add list" [1; 2; 3] (to_list l);

	let l2 = add_list l [10; 20; 30] in 
	Alcotest.(check (list int)) "Test add list" [10; 20; 30; 1; 2; 3] (to_list l2)


let test_of_list () = 
	let l = [1; 2; 3; 4; 5] in 
  	let im = of_list l in 
  	Alcotest.(check int) "Test of_list hd" 1 (hd im);
  	Alcotest.(check int) "Test of_list last" 5 (last im)


let test_to_list () =
	let l = range 1 3 in
  	Alcotest.(check (list int)) "Test to_list" [1; 2; 3] (to_list l) 


let test_of_list_map () =
	let l = of_list_map ~f:(fun x -> x * 2) [1; 2; 3] in
  	Alcotest.(check (list int)) "Test of_list_map" [2; 4; 6] (to_list l) 


let test_add_array () = 
	let a = [|1; 2|] in
  	let l = add_array empty a in
  	Alcotest.(check (list int)) "Test add array" [1; 2] (to_list l)


let test_of_array () = 
	let a = [|1; 2; 3|] in
  	Alcotest.(check (list int)) "Test of_array" [1; 2; 3] (to_list (of_array a))


let test_to_array () = 
	let l = of_list [1; 2; 3] in
  	Alcotest.(check (array int)) "Test to_array" [|1; 2; 3|] (to_array l)


let test_add_iter () =
	let my_iter f = List.iter f [1; 2] in
    let l = add_iter empty my_iter in
    Alcotest.(check (list int)) "Test add iter" [1; 2] (to_list l)


let test_of_iter () = 
	let my_iter f = List.iter f [1; 2; 3] in
    let l = of_iter my_iter in
    Alcotest.(check (list int)) "Test of_iter" [1; 2; 3] (to_list l)

let test_to_iter () = 
	let l = of_list [1; 2; 3] in
    let my_iter = to_iter l in
    let acc = ref [] in my_iter (fun x -> acc := x :: !acc);
    Alcotest.(check (list int)) "Test to_iter" [1; 2; 3] (List.rev !acc)

let test_add_gen () = 
	let state = ref 0 in
    let gen () = if !state < 2 then (incr state; Some !state) else None in
    let l = add_gen empty gen in
    Alcotest.(check (list int)) "Test add_gen" [1; 2] (to_list l)


let test_of_gen () = 
	let state = ref 0 in
    let gen () = if !state < 3 then (incr state; Some !state) else None in
    let l = of_gen gen in
    Alcotest.(check (list int)) "Test of_gen" [1; 2; 3] (to_list l)


let test_to_gen () = 
    let l = of_list [10; 20; 30] in
    let gen = to_gen l in
    
    Alcotest.(check (option int)) "Test to_gen" (Some 10) (gen ());
    Alcotest.(check (option int)) "Test to_gen" (Some 20) (gen ());
    Alcotest.(check (option int)) "Test to_gen" (Some 30) (gen ());
    Alcotest.(check (option int)) "Test to_gen" None (gen ());
    Alcotest.(check (option int)) "Test to_gen" None (gen ())


let test_infix_cons () = 
	let l = of_list [2; 3] in
	Alcotest.(check (list int)) "Test (@+)" [1; 2; 3] (to_list (1 @+ l));
	Alcotest.(check (list int)) "Test (@+) igual a cons" (to_list (cons 1 l)) (to_list (1 @+ l))


let test_infix_flat_map () = 
	let l = of_list [1; 2; 3] in
	let f x = of_list [x; x * 10] in
	Alcotest.(check (list int)) "Test (>>=)" [1; 10; 2; 20; 3; 30] (to_list (l >>= f));
	Alcotest.(check (list int)) "Test (>>=)" (to_list (flat_map f l)) (to_list (l >>= f))


let test_infix_map () = 
	let l = of_list [1; 2; 3] in
	let f x = x * 2 in
	Alcotest.(check (list int)) "Test (>|=)" [2; 4; 6] (to_list (l >|= f));
	Alcotest.(check (list int)) "Test (>|=)" (to_list (map ~f l)) (to_list (l >|= f))


let test_infix_app () = 
	let f1 = (fun x -> x + 1) in 
	let f2 = (fun x -> x * 2) in 
	let l = of_list [1; 2; 3] in 
	let funs = of_list [f1; f2] in 
	Alcotest.(check (list int)) "Test (<*>)" [2; 3; 4; 2; 4; 6] (to_list (funs <*> l));
	Alcotest.(check (list int)) "Test (<*>)" (to_list (app funs l)) (to_list (funs <*> l))


let test_infix_range () = 
	Alcotest.(check (list int)) "Test (--)" [2; 3; 4; 5] (to_list (2 -- 5));
	Alcotest.(check (list int)) "Test (--)" [5; 4; 3; 2] (to_list (5 -- 2));
	Alcotest.(check (list int)) "Test (--)" (to_list (range 2 5)) (to_list (2 -- 5))


let test_infix_range_open () = 
	Alcotest.(check (list int)) "Test (--^)" [2; 3; 4] (to_list (2 --^ 5));
	Alcotest.(check bool) "Test (--^)" true (is_empty (2 --^ 2))


let () =
	let open Alcotest in
	run "Tests" [
		"Functions", [
			test_case "Function empty" `Quick test_empty;
			test_case "Function is_empty" `Quick test_is_empty;
			test_case "Function cons" `Quick test_cons;
			test_case "Function return " `Quick test_return;
			test_case "Function map" `Quick test_map;
			test_case "Function mapi" `Quick test_mapi;
			test_case "Function hd" `Quick test_hd;
			test_case "Function last" `Quick test_last;
			test_case "Function tl" `Quick test_tl;
			test_case "Function front" `Quick test_front;
			test_case "Function front_exn" `Quick test_front_exn;
			test_case "Function length" `Quick test_length;
			test_case "Function get" `Quick test_get;
			test_case "Function get_exn" `Quick test_get_exn;
			test_case "Function set" `Quick test_set;
			test_case "Function remove" `Quick test_remove;
			test_case "Function get_and_remove_exn" `Quick test_get_and_remove_exn;
			test_case "Function append" `Quick test_append;
			test_case "Function filter" `Quick test_filter;
			test_case "Function filter_map" `Quick test_filter_map;
			test_case "Function flat_map" `Quick test_flat_map;
			test_case "Function flatten" `Quick test_flatten;
			test_case "Function app" `Quick test_app;
			test_case "Function take" `Quick test_take;
			test_case "Function take_while" `Quick test_take_while;
			test_case "Function drop" `Quick test_drop;
			test_case "Function drop_while" `Quick test_drop_while;
			test_case "Function take_drop" `Quick test_take_drop;
			test_case "Function iter" `Quick test_iter;
			test_case "Function iteri" `Quick test_iteri;
			test_case "Function fold" `Quick test_fold;
			test_case "Function fold_rev" `Quick test_fold_rev;
			test_case "Function rev_map" `Quick test_rev_map;
			test_case "Function equal" `Quick test_equal;
			test_case "Function compare" `Quick test_compare;
		];
		"Utils", [
			test_case "Function make" `Quick test_make;
			test_case "Function repeat" `Quick test_repeat;
			test_case "Function range" `Quick test_range;
		];
		"Conversions", [
			test_case "Function add_list" `Quick test_add_list;
			test_case "Function of_list" `Quick test_of_list;
			test_case "Function to_list" `Quick test_to_list;
			test_case "Function of_list_map" `Quick test_of_list_map;
			test_case "Function of_array" `Quick test_of_array;
			test_case "Function add_array" `Quick test_add_array;
			test_case "Function to_array" `Quick test_to_array;
			test_case "Function add_iter" `Quick test_add_iter;
			test_case "Function of_iter" `Quick test_of_iter;
			test_case "Function to_iter" `Quick test_to_iter;
			test_case "Function add_gen" `Quick test_add_gen;
			test_case "Function of_gen" `Quick test_of_gen;
			test_case "Function to_gen" `Quick test_to_gen;
		];
		"Infix", [
			test_case "Infix Cons" `Quick test_infix_cons;
			test_case "Infix Flat Map" `Quick test_infix_flat_map;
			test_case "Infix Map" `Quick test_infix_map;
			test_case "Infix App" `Quick test_infix_app;
			test_case "Infix Range" `Quick test_infix_range;
			test_case "Infix Range Open" `Quick test_infix_range_open;
		];
	]

