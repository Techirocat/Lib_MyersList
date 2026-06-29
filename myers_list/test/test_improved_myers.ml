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
  Alcotest.(check (option int)) "Teste se o get retorna valor certo" (Some 1) (get myersList 4);
  Alcotest.(check (option int)) "Teste se o get retorna valor certo" (Some 5) (get myersList 0);


let test_set () = 
  let list = [0; 1; 2; 3; 4; 5] in 
  let myersList = of_list list in 
  let newL = set myersList 4 40 in 
  Alcotest.(check (option int)) "Testa set" (Some 40) (get newL 4);
  Alcotest.(check (list int)) "Testa set" ([0; 40; 2; 3; 4; 5]) (to_list newL)


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
    "Testes Set", [
      test_case "Função Set" `Quick test_set;
    ];
  ];;