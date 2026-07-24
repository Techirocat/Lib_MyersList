open Core
open Core_bench
open Myers_list

let get = function
  | Some v -> v
  | None -> raise (Invalid_argument "option is None")

let rec cons_n_elements n current_list =
  if n = 0 then current_list
  else cons_n_elements (n - 1) (Iarass3.cons n current_list)

let lookup_all n list = 
  let rec aux i = 
    if i = n then 
      ()
    else begin 
      ignore (Iarass3.get_cost list i);
      aux (i + 1)
    end
  in aux 0 


let () = 
  let conf = Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 200) () in
  
  let testes = [ 
    Bench.Test.create_indexed
      ~name:"Cons"
      ~args:[1; 10; 100; 1_000; 10_000; 100_000; 1_000_000; 10_000_000]
      (fun len -> 
         Staged.stage (fun () -> 
           ignore (cons_n_elements len Iarass3.empty)
         )
      );

    Bench.Test.create_indexed
      ~name:"Lookup (len / 2)"
      ~args:[10_000; 100_000; 1_000_000; 10_000_000]
      (fun len -> 
          let list = cons_n_elements len Iarass3.empty in 
          let target = len / 2 in 
          Staged.stage (fun () -> ignore (Iarass3.get_cost list target))
      );

    Bench.Test.create_indexed
      ~name:"Lookup (Last index)"
      ~args:[10_000; 100_000; 1_000_000; 10_000_000]
      (fun len -> 
          let list = cons_n_elements len Iarass3.empty in 
          let target = len - 1 in 
          Staged.stage (fun () -> ignore (Iarass3.get_cost list target))
      );


    Bench.Test.create_indexed
      ~name:"Lookup All"
      ~args:[10_000; 100_000; 1_000_000; 10_000_000]
      (fun len -> 
          let list = cons_n_elements len Iarass3.empty in  
          Staged.stage (fun () -> ignore (lookup_all len list ))
      )

 

  ]
  in
  
  let mes = Bench.measure ~run_config:conf testes in
  
  let ana = 
    List.map mes ~f:(fun m -> 
      let analise = Bench.analyze ~analysis_configs:Bench.Analysis_config.default m in
      Or_error.ok_exn analise 
    ) 
  in
  
  Bench.display ana
