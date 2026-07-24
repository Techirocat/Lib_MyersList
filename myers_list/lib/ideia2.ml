type 'a cell = 
  | Normal of {next : 'a cell; jump : 'a cell; length : int; value : 'a; heigth : int}
  | Skip of {next : 'a cell; jump : 'a cell; length : int; value : 'a; heigth : int; shortcut : 'a cell}

type 'a wrap = {head : 'a cell;  last : 'a cell; list : int list}

type 'a t = 
  | Empty 
  | Wrap of { head : 'a cell; last : 'a cell; list : 'a cell list }

let empty = Empty 

let init' v : 'a cell = 
  let rec c : 'a cell = Skip {next = c; jump = c; length = 0; value = v; heigth = 0; shortcut = c} in c 

let get_heigth (c : 'a cell) = 
  match c with 
  | Normal c -> c.heigth
  | Skip c -> c.heigth

let get_length (c: 'a cell) = 
  match c with 
  | Normal c -> c.length
  | Skip c -> c.length

let get_value c = 
  match c with 
  | Normal c -> c.value 
  | Skip c -> c.value

let get_next (c : 'a cell) = 
  match c with 
  | Normal c -> c.next 
  | Skip c -> c.next 

let get_jump (c : 'a cell) = 
  match c with 
  | Normal c -> c.jump 
  | Skip c -> c.jump 

let get_shortcut (c : 'a cell) = 
  match c with 
  | Normal c -> failwith "Erro: Nós normais não tem shortcut"
  | Skip c -> c.shortcut

let init v : 'a t = 
  let c : 'a cell = init' v in 
  Wrap {head = c; last = c; list = [c]} 


let return v : 'a t = init v 

let is_empty wrap = 
  match wrap with 
  | Empty -> true 
  | Wrap _ -> false



let rec lookup (c : 'a cell) (l : int) : 'a cell =
  if (get_length c) = l then 
    c 
  else 
    match c with 
    | Normal _ -> 
        let j = get_jump c in 
        let c' = if (get_length j) < l then get_next c else j in 
        lookup c' l 
    | Skip _ ->
        let s = get_shortcut c in 
        let j = get_jump c in 
        let c' = 
          if (get_length s) > l then s 
          else if (get_length j) < l then get_next c else j in
          lookup c' l 
  
      

let cons v wrap = 
  match wrap with 
  | Empty -> init v
  | Wrap {head = h; last = l; list = heigths} ->
      let sigma = 2  in (*Fixar o sigma a 2 só por enquanto para verificar se os nós skips irão fazer efeito*)
      let c_height = 
        match heigths with 
        | [] -> 0
        | [x] -> 0 
        | x::y::acc when get_heigth x = get_heigth y -> get_heigth x + 1
        | x::y::acc when get_heigth x <> get_heigth y -> 0
        | _ -> failwith "Não estou a ver que outros casos são possíveis aqui, mas o compilador está a chatear XD"
      in
      let acc = 
        match heigths with 
        | [] -> failwith "Acredito que nunca devia estar vazia" 
        | [x] -> heigths
        | x :: y :: acc -> 
            if get_heigth x = get_heigth y then 
              acc
            else 
              heigths
      in
      let h_len = get_length h in 
      let h_hj = h_len - (get_length (get_jump h)) in 
      let c_jump = 
          if h_hj == 1 || h_hj == (get_length (get_next l)) - (get_length (get_jump (get_next l))) then 
            get_next l
          else 
            l 
      in 
      let c = 
        if c_height mod sigma = 0 then 
          let new_uncle = get_jump l in
          Skip {next = h; jump = c_jump; length = h_len + 1; value = v; heigth = c_height; shortcut = new_uncle}
        else 
          Normal {next = h; jump= c_jump; length = h_len + 1; value = v; heigth = c_height}
      in
      let new_last = 
          if h_hj == 1 || h_hj == (get_length (get_next l)) - (get_length (get_jump (get_next l))) then 
            get_jump l
          else 
            c
      in Wrap {head = c; last = new_last; list = (c :: acc)}


let rec lookup_cost c l cost =   
  if (get_length c) = l then 
    cost 
  else 
    match c with 
    | Normal _ -> 
        let j = get_jump c in 
        let c' = if (get_length j) < l then get_next c else j in 
        lookup_cost c' l (cost + 1) 
    | Skip _ ->
        let s = get_shortcut c in 
        let j = get_jump c in 
        let c' = 
          if (get_length s) > l then s 
          else if (get_length j) < l then get_next c else j in
          lookup_cost c' l (cost + 1)


let get_cost wrap i = 
  match wrap with 
  | Empty -> failwith ""
  | Wrap {head = h; last = _; list = _} -> 
    let h_len = get_length h in 
    if i < 0 || i > h_len then 
      failwith ""
    else 
      let target = h_len - i in 
      lookup_cost h target 0


let add_list wrap l = List.fold_left (fun acc v -> cons v acc) wrap (List.rev l)

let of_list l = add_list empty l

let print_cell c : unit = 
  match c with 
  | Normal c -> 
      Printf.printf "{\n Normal Cell\n Value = %d\n Next = %d\n Jump = %d\n}" c.value (get_value c.next) (get_value c.jump)
  | Skip c -> 
      Printf.printf "{\n Skip Cell\n Value = %d\n Next = %d\n Jump = %d\n Skip = %d\n}" c.value (get_value c.next) (get_value c.jump) (get_value c.shortcut)

let print_IARASS wrap = 
  match wrap with
  | Empty -> Printf.printf "Empty"
  | Wrap {head = h; last = _} -> 
      let rec aux c = 
        if get_length c = 0 then
          (print_cell c)
        else begin 
          print_cell c; 
          aux (get_next c)
        end
      in aux h 

let range i j = 
	let rec aux i j acc = 
		if i = j then 
			cons i acc 
		else if i < j then 
			aux i (j-1) (cons j acc) 
		else 
			aux i (j+1) (cons j acc)
  	in aux i j empty 


          
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
let max_cost, total_sum = find_stats 0 0 0.0 in let avg_cost = total_sum /. (float_of_int current_n) in        Printf.printf "%d %d %.4f\n" current_n max_cost avg_cost; let next_list = cons current_n current_list in run_tests next_list (current_n + 1) end in let initial_list = cons 0 empty in 
    run_tests initial_list 1
*)

   
  

