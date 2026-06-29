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
  ];;