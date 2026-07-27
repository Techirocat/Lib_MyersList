open Myers_list
open Advanced_myers
open Core.Staged
open Core_bench

(*
let get = function
    | Some v -> v
    | None -> raise (Invalid_argument "option is None")

let () = 
    let conf = Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 1) () in
    let test =
    [ 
        Bench.Test.create_indexed
            ~name:"Create"
            ~args:[1;10;100;1000;10000;100000;1000000]
            (fun len -> stage (fun () -> Printf.printf "create %d\n%!" len; create_test len Nil));
        Bench.Test.create_indexed
            ~name:"Lookup Middle"
            ~args:[10;100;1000;10000;100000]
            (fun len -> 
                let list = create_test len Nil in
                stage (fun () -> Printf.printf "lookup middle %d\n%!" len; lookup_t list (len/2)));
        Bench.Test.create_indexed
            ~name:"Lookup Last"
            ~args:[10;100;1000;10000;100000]
            (fun len -> 
                let list = create_test len Nil in
                stage (fun () -> Printf.printf "lookup last %d\n%!" len; lookup_t list 1));
        Bench.Test.create_indexed
            ~name:"Lookup All"
            ~args:[10;100;1000;10000;100000;1000000]
            (fun len ->
                let list = create_test len Nil in
                let rec lookup_all list len count =
                    let _c = lookup_t list count in
                    if len = count then
                        ()
                    else
                        lookup_all list len (count+1) in
                stage (fun () -> Printf.printf "lookup all %d\n%!" len; lookup_all list len 1));
        Bench.Test.create_indexed
            ~name:"Cons (Normal)"
            ~args:[6;993;9993;99993]
            (fun len ->
                let list = create_test (len-1) Nil in
                stage (fun () -> Printf.printf "cons normal %d\n%!" len; cons 1 list));
        Bench.Test.create_indexed
            ~name:"Cons (Skip)"
            ~args:[7;994;9994;99994]
            (fun len ->
                let list = create_test (len-1) Nil in
                stage (fun () -> Printf.printf "cons skip %d\n%!" len; cons 1 list));
        Bench.Test.create_indexed
            ~name:"Cons (Leaf)"
            ~args:[8;9;995;9996;99995]
            (fun len ->
                let list = create_test (len-1) Nil in
                stage (fun () -> Printf.printf "cons leaf %d\n%!" len; cons 1 list));
    ] in
    let mes = Bench.measure ~run_config:conf test in
    let ana = List.fold_left (fun l m -> (Result.get_ok (Bench.analyze ~analysis_configs:Bench.Analysis_config.default m)):: l) [] (List.rev mes) in
    Bench.display ana

*)
