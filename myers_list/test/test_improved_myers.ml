open Myers_list
open Improved_myers

let test_empty () =
  let list = empty in 
  Alcotest.(check bool) "Verifica se está vazio" true (is_empty list)

let test_cons_empty () = 
  let list = cons 10 empty in
  Alcotest.(check int) "Vereficar se adiciona num Wrap vazio" 10 (hd list)

let test_cons () = 
  let list = cons 20 (cons 10 empty) in 
  Alcotest.(check int) "Vereficar se adiciona num Wrap vazio" 20 (hd list)

let test_get () = 
  let list = [0; 1; 2; 3; 4; 5] in 
  let myersList = of_list list in 
  Alcotest.(check (option int)) "Testa se o get retorna valor certo" (Some 1) (get myersList 4);
  Alcotest.(check (option int)) "Testa se o get retorna valor certo" (Some 5) (get myersList 0)

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
      test_case "Função rev" `Quick test_rev;
      test_case "Função rev em lista vazia" `Quick test_rev_empty;
      test_case "Função rev_map" `Quick test_rev_map;
      test_case "Função rev_map em lista vazia" `Quick test_rev_map_empty;
    ];
  ]
