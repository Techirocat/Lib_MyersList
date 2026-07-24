
type 'a cell = {
        next : 'a cell; 
        jump : 'a cell; 
        length : int; 
        value : 'a; 
        heigth : int; 
        shortcut : 'a cell
    }

type 'a t = 
  | Empty 
  | Wrap of {
       head : 'a cell; 
       last : 'a cell
    }

let pow2 n = 1 lsl n
[@@inline]

let empty = Empty 

let init' v : 'a cell = 
  let rec c : 'a cell = {next = c; jump = c; length = 0; value = v; heigth = 0; shortcut = c} in c 


let init v : 'a t = 
  let c : 'a cell = init' v in 
  Wrap {head = c; last = c} 
[@@inline]


let return v : 'a t = init v 

let is_empty wrap = 
  match wrap with 
  | Empty -> true 
  | Wrap _ -> false


let is_not_leaf h l =
    let xj = h.length - h.jump.length in
    let rj = l.next.length - l.next.jump.length in
    (not (h.length = 0)) && (xj = 1 || xj = rj)

let is_leaf hn l = not (is_not_leaf hn l)


let cons v wrap = 
  match wrap with 
  | Empty -> init v
  | Wrap {head = h; last = l} ->
      let sigma = 2 in (*Fixar o sigma a 2 só por enquanto para verificar se os nós skips irão fazer efeito*)
      let h_len = h.length in 
      let h_hj = h_len - h.jump.length in 
      let c_jump = 
          if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then 
            l.next
          else 
            l 
      in 
      let c_height = 
      let leaf = is_leaf h l in 
        if leaf then 
          0
        else 
          h.heigth + 1
      in 
      let c = 
        if c_height mod sigma = 0 then 
          if c_height <> 0 then 
            let new_uncle =  l.jump in 
              {next = h; jump = c_jump; length = h_len + 1; value = v; heigth = c_height; shortcut = new_uncle}
          else 
            let new_uncle = c_jump.jump.jump in
              {next = h; jump = c_jump; length = h_len + 1; value = v; heigth = c_height; shortcut = new_uncle}
        else 
          {next = h; jump= c_jump; length = h_len + 1; value = v; heigth = c_height; shortcut = c_jump}
      in
      let new_last = 
          if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then l.jump else c
      in Wrap {head = c; last = new_last}

let pick c1 c2 l = 
  if c2.length >= l then 
    c2 
  else 
    c1   
[@@inline]

let rec lookup_cost c l cost =   
  if c.length = l then 
    cost
  else if c.shortcut.length >= l then 
    lookup_cost c.shortcut l (cost + 1)
  else if c.jump.length >= l then 
    lookup_cost c.jump l (cost + 1)
  else 
    lookup_cost c.next l (cost + 1)



(*
let rec lookup_cost c l cost =   
  if c.length = l then 
    cost 
  else 
    match c.shortcut with 
    | Normal -> lookup_cost (pick c.next c.jump l) l (cost + 1)  
    | Skip s -> 
        lookup_cost (pick (pick c.next c.jump l) s l) l (cost + 1)
*)
(*
let rec lookup c l =   
  if c.length = l then 
    c.value 
  else 
    match c.shortcut with 
    | Normal -> lookup (pick c.next c.jump l) l  
    | Skip s -> 
        lookup (pick (pick c.next c.jump l) s l) l

*)


let get_cost wrap i = 
  match wrap with 
  | Empty -> failwith ""
  | Wrap {head = h; last = _} -> 
    let h_len = h.length in 
    if i < 0 || i > h_len then 
      failwith ""
    else 
      let target = h_len - i in 
      lookup_cost h target 0


(*

let () = 
    let lmax = 100_000 in 
    let rec run_tests current_list current_n =  
      if current_n <= lmax then begin 

        let rec find_stats i max_so_far sum_so_far = 
          if i >= current_n then 
            (max_so_far, sum_so_far)
          else 
            let cost = get_cost current_list i in 
            find_stats (i + 1) (max max_so_far cost) (sum_so_far +. float_of_int cost)
        in

        let max_cost, total_sum = find_stats 0 0 0.0 in 
        let avg_cost = total_sum /. (float_of_int current_n) in        
        Printf.printf "length: %d worst: %d avg: %.4f\n%!" current_n max_cost avg_cost;

        let next_list = cons current_n current_list in 
        run_tests next_list (current_n + 1)
      end
    in
    let initial_list = cons 0 empty in 
    run_tests initial_list 1

*)
(*

let () = 
  let lmax = 1_000_000 in 
  
  let rec run_tests current_list current_n =  
    if current_n <= lmax then begin 

      let rec find_stats i max_so_far sum_so_far = 
        if i >= current_n then 
          (max_so_far, sum_so_far)
        else 
          let cost = get_cost current_list i in 
          find_stats (i + 1) (max max_so_far cost) (sum_so_far +. float_of_int cost)
      in

      let max_cost, total_sum = find_stats 0 0 0.0 in 
      let avg_cost = total_sum /. (float_of_int current_n) in        
      
      Printf.printf "length: %d worst: %d avg: %.4f\n%!" current_n max_cost avg_cost;

      let next_list = cons current_n current_list in 
      run_tests next_list (current_n + 1)
    end
  in
  
  let initial_list = cons 0 empty in 
  run_tests initial_list 1

  *)



(*


let () =
  let lmax = 100_000 in 
  
  (* Função auxiliar para inserir vários elementos seguidos sem testar *)
  let rec build_list acc i end_val =
    if i = end_val then acc
    else build_list (cons i acc) (i + 1) end_val
  in
  
  let rec run_tests current_list current_n =  
    if current_n <= lmax then begin 

      let rec find_stats i max_so_far sum_so_far = 
        if i >= current_n then 
          (max_so_far, sum_so_far)
        else 
          let cost = get_cost current_list i in 
          find_stats (i + 1) (max max_so_far cost) (sum_so_far +. float_of_int cost)
      in

      let max_cost, total_sum = find_stats 0 0 0.0 in 
      let avg_cost = total_sum /. (float_of_int current_n) in        
      
      Printf.printf "length: %d worst: %d avg: %.4f\n%!" current_n max_cost avg_cost;

      let next_n = current_n + 1 in
      let next_list = build_list current_list current_n next_n in 
      
      run_tests next_list next_n
    end
  in
  
  let start_length = 1 in
  let initial_list = build_list empty 0 start_length in 
  
  run_tests initial_list start_length
  *)