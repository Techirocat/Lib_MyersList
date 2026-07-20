open Myers_list
open Advanced_myers
open Core_bench

let get = function
    | Some v -> v
    | None -> raise (Invalid_argument "option is None")

let () = 
    (*
    Command_unix.run (Bench.make_command [
        Bench.Test.create_indexed
            ~name:"Create"
            ~args:[1;10;100;1000;10000;100000]
            (fun len -> Staged.stage (fun () -> ignore (create_test len Nil)));
        Bench.Test.create_indexed
            ~name:"Lookup_Half"
            ~args:[10;100;1000;10000;100000]
            (fun len -> 
                let l = create_test len Nil in
                Staged.stage (fun () -> 
                    lookup_t l (len/2)
                )
            );
    ])
    *)
    let conf = Bench.Run_config.create ~quota:(Bench.Quota.Num_calls 250) () in
    let l = create_test 1000 Nil in
    let mes =
    [ Bench.Test.create
        ~name:"Lookup"
        (fun len -> 
            lookup_t l 500
        ) ]
    |> Bench.measure ~run_config:conf in
    let ana = Bench.analyze ~analysis_configs:Bench.Analysis_config.default ((List.hd mes)) in
    Bench.display [(Result.get_ok ana)]
        


