open Myers_list
open Improved_myers

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
	Alcotest.(check int) "Test cons" 2 (length li); 
	Alcotest.(check int) "Test cons" 20 (hd li);
	Alcotest.(check int) "Test cons" 10 (last li)


let test_return () = 
	let l = return 10 in 
	Alcotest.(check int) "Test return" 1 (length li); 
	Alcotest.(check int) "Test return" 10 (hd l)


let test_map () =
	let f = (fun x -> x * 2) in 
	let l = of_list [1; 2; 3; 4; 5] in 
	let expected = [2; 4; 6; 8 ; 10] in 
	Alcotest.(check (list int)) "Test map" expected (to_list (map f l));
	Alcotest.(check (list int)) "Test map" [] to_list((map f empty))


let test_mapi () = 
	let f = (fun i x -> x * i) in 
	let l = of_list [1; 2; 3; 4; 5] in 
	let expected = [0; 2; 6; 12 ; 20] in 
	Alcotest.(check (list int)) "Test mapi" expected (to_list (map f l));
	Alcotest.(check (list int)) "Test mapi" [] to_list((map f empty))


let test_hd () = 
	let l = of_list [99; 2; 3] in
  	Alcotest.(check int) "Test hd" 99 (hd l)

let test_last () = 
	let l = of_list [99; 2; 3] in
  	Alcotest.(check int) "Test last" 3 (last l)

let test_tl () =
	let l = of_list [1; 2; 3] in
  	let tail = tl l in
  	Alcotest.(check (list int)) "Test tl" [2; 3] (to_list tail)

let test_front () = 
	let l = empty in 
	let l1 = cons 1 l in 
	let l2 = cons 2 l in 
  	Alcotest.(check (option int)) "Test front" None (front l);
  	Alcotest.(check (option int)) "Test front" (Some 1) (front l1);
  	Alcotest.(check (option int)) "Test front" (Some 2) (front l2);
  	


let test_front_exn () = 

let test_length () = 
	let l = of_list [1; 2; 3; 4; 5] in
  	Alcotest.(check int) "Test length" 5 (length l);
  	Alcotest.(check int) "Test length" 0 (length empty)

let test_get () = 
	let l = of_list [0; 1; 2; 3; 4; 5] in 
  	Alcotest.(check (option int)) "Get 4" (Some 4) (get l 4);
  	Alcotest.(check (option int)) "Get 0" (Some 0) (get l 0);
  	Alcotest.(check (option int)) "Get invalid index" None (get l (-1));
  	Alcotest.(check (option int)) "Get invalid index" None (get l 13);
	Alcotest.(check (option int)) "Get in Empty List" None (get empty 1)


let test_get_exn () = 

let test_set () = 
	let l = of_list [10; 20; 30; 40] in
  	Alcotest.(check (list int)) "Test set" [99; 20; 30; 40] (to_list (set l 0 99));
	Alcotest.(check (list int)) "Test set" [10; 99; 30; 40] (to_list (set l 1 99));
	Alcotest.(check (list int)) "Test set" [10; 20; 99; 40] (to_list (set l 2 99));
	Alcotest.(check (list int)) "Test set" [10; 20; 30; 99] (to_list (set l 3 99));
	Alcotest.(check (list int)) "Test set" [10; 99; 30; 40] (to_list (set empty 1 99))


let test_remove () = 

let test_get_and_remove_exn () = 

let test_append () = 

let test_filter () = 
	let lst = of_list [1; 2; 3; 4; 5; 6] in
  	let fil = filter (fun x -> x mod 2 = 0) lst in
  	Alcotest.(check (list int)) "Test filter" [2; 4; 6] (to_list fil)


let test_filter_map () =

let test_flatten () = 

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
  	Alcotest.(check (list int)) "Test take" expected (to_list (take 3 l))

	
let test_take_while () =

let test_drop () = 

let test_drop_while () = 


let test_take_drop () = 

let test_iter () = 

let test_iteri () = 

let test_fold () = 
	let l = of_list [1; 2; 3; 4] in
  	let sum = fold (fun acc x -> acc + x) 0 l in
  	Alcotest.(check int) "Fold deve somar todos os elementos" 10 sum

let test_fold_rev () = 

let test_rev_map () = 

let test_rev () = 
	let l = of_list [0; 1; 2; 3; 4; 5] in
  	let reversed = rev l in
 	Alcotest.(check (list int)) "Test rev" [5; 4; 3; 2; 1; 0] (to_list reversed);
  	Alcotest.(check bool) "Test rev" true (is_empty (rev empty))


let test_equal () = 
	let l1 = of_list [1; 2; 3] in
  	let l2 = of_list [1; 2; 3] in
  	let l3 = of_list [10; 20; 30] in
  	Alcotest.(check bool) "Test equals" true (equal (=) l1 l2);
  	Alcotest.(check bool) "Test equals" false (equal (=) l1 l3)

let test_compare () = 

let test_make () = 

let test_repeat () = 

let test_range () = 
	let lst = range 2 5 in
 	Alcotest.(check (list int)) "Test Range" [2; 3; 4; 5] (to_list lst)

let test_add_list () = 
	

let test_of_list () = 

let test_to_list () = 

let test_of_list_map () = 

let test_add_array () = 

let test_of_array () = 

let test_to_array () = 

let test_add_iter () = 

let test_of_iter () = 

let test_to_iter () = 

let test_add_gen () = 

let test_of_gen () = 




let () =
	let open Alcotest in
	run "Tests" [
		"A", [
			test_case "Função Empty" `Quick test_empty;
		];
	]


(*
let test_empty () =
	let list = empty in 
	Alcotest.(check bool) "Verifica se está vazio" true (is_empty list)



let test_cons_empty () = 
	let list = cons 10 empty in
	Alcotest.(check int) "Vereficar se adiciona num Wrap vazio" 10 (hd list)

let test_cons () = 
	let list = cons 20 (cons 10 empty) in 
	Alcotest.(check int) "Vereficar se adiciona num Wrap vazio" 20 (hd list)

let test_of_list () = 
	let list = [1; 2; 3; 4; 5] in 
	let im = of_list list in 

	Alcotest.(check (int)) "Teste de of_list" 1 (hd im);
	Alcotest.(check (int)) "Teste de of_list" 5 (last im)



let test_get () = 
	let list = [0; 1; 2; 3; 4; 5] in 
	let myersList = of_list list in 
	Alcotest.(check (option int)) "Testa se o get retorna valor certo" (Some 4) (get myersList 4);
	Alcotest.(check (option int)) "Testa se o get retorna valor certo" (Some 0) (get myersList 0);
	Alcotest.(check (option int)) "Testa se o get retorna valor certo" (Some 5) (get myersList 5);
	Alcotest.(check (option int)) "Testa se o get retorna valor certo" (None) (get myersList (-1));
	Alcotest.(check (option int)) "Teste Get index invalido" (None) (get myersList 13)



let test_rev () =
	let list = [0; 1; 2; 3; 4; 5] in
	let myersList = of_list list in
	let reversed = rev myersList in
	Alcotest.(check (list int)) "Testa rev" [5; 4; 3; 2; 1; 0] (to_list reversed)

let test_rev_empty () =
	let myersList = empty in
	let reversed = rev myersList in
	Alcotest.(check bool) "Testa rev em lista vazia" true (is_empty reversed)

let test_rev_map () =
	let list = [0; 1; 2; 3; 4; 5] in
	let myersList = of_list list in
	let reversed = rev_map (fun x -> x * 10) myersList in
	Alcotest.(check (list int)) "Testa rev_map" [50; 40; 30; 20; 10; 0] (to_list reversed)

let test_rev_map_empty () =
	let myersList = empty in
	let reversed = rev_map (fun x -> x * 10) myersList in
	Alcotest.(check bool) "Testa rev_map em lista vazia" true (is_empty reversed)

let teste_app () = 
	let f1 = (fun x -> x + 1) in 
	let f2 = (fun x -> x * 2) in 
	let lista = [1; 2; 3; 4; 5] in 
	let im = of_list lista in 
	let funs = of_list [f1; f2] in 

	let expected = [2; 3; 4; 5; 6; 2; 4; 6; 8; 10] in 

	Alcotest.(check (list int)) "Testa app" expected (to_list (app funs im)) 



let () =
	let open Alcotest in
	run "Testes" [
		"Testes Empty", [
			test_case "Função Empty" `Quick test_empty;
		];
		"Testes Cons", [
			test_case "Adicionar a lista vazia" `Quick test_cons_empty;
			test_case "Adicionar a lista" `Quick test_cons;
		];
		"Testes Get", [
			test_case "Função Get" `Quick test_get;
		];
		"Testes Rev", [
		 (* test_case "Função rev" `Quick test_rev;
			test_case "Função rev em lista vazia" `Quick test_rev_empty;
			test_case "Função rev_map" `Quick test_rev_map;
			test_case "Função rev_map em lista vazia" `Quick test_rev_map_empty;*)
		];
		"Testes app", [
			test_case "Função app" `Quick teste_app;
		];
		"Testes of_list", [
			test_case "Função of_list" `Quick test_of_list
		]
	]

*)
