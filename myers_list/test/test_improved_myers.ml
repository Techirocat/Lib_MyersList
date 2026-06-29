(* Assumindo que o teu código original está num módulo chamado MyTree *)
open Myers_list
open Improved_myers

let test_empty () =
  let list = empty in 
  Alcotest.(check bool) "Verifica se está vazio" true (is_empty list)


let () =
  let open Alcotest in
  run "Testes" [
    "Testes Empty", [
      test_case "Função Empty" `Quick test_empty;
    ];
  ]