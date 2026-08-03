
type 'a cell = 
  | Normal of {
        next : 'a cell; 
        jump : 'a cell; 
        length : int; 
        value : 'a; 
        height : int
    }
  | Skip of {
        next : 'a cell; 
        jump : 'a cell; 
        length : int; 
        value : 'a; 
        height : int; 
        shortcut : 'a cell
    }

type 'a t = 
  | Empty of int 
  | Wrap of {
       head : 'a cell; 
       last : 'a cell;
       sigma : int;
    }

let empty = Empty 2

let empty_with_sigma sigma = 
  if sigma <= 0 then 
    invalid_arg "Invalid Sigma" 
  else 
    Empty sigma

let init' v : 'a cell = 
  let rec c : 'a cell = Skip {next = c; jump = c; length = 0; value = v; height = 0; shortcut = c} in c 

let get_height (c : 'a cell) = 
  match c with 
  | Normal c -> c.height
  | Skip c -> c.height
[@@inline]


let get_length (c: 'a cell) = 
  match c with 
  | Normal c -> c.length
  | Skip c -> c.length
[@@inline]

let get_next (c : 'a cell) = 
  match c with 
  | Normal c -> c.next 
  | Skip c -> c.next 
[@@inline]


let get_jump (c : 'a cell) = 
  match c with 
  | Normal c -> c.jump 
  | Skip c -> c.jump 
[@@inline]

let get_shortcut (c : 'a cell) = 
  match c with 
  | Normal _ -> invalid_arg "get_shortcut: cell has no shortcut (not a Skip cell)"
  | Skip c -> c.shortcut
[@@inline]

let get_last wrap = 
  match wrap with
  | Empty _ -> failwith "Error"
  | Wrap {head = _; last = l} -> l
[@@inline]

let get_sigma wrap = 
  match wrap with 
  | Empty s -> s 
  | Wrap {sigma; _} -> sigma
[@@inline]

let get_value (c : 'a cell) = 
  match c with 
  | Normal c -> c.value
  | Skip c -> c.value
[@@inline]

let init ?(sigma= 2) v : 'a t = 
  if sigma <= 0 then 
    invalid_arg "Invalid Sigma"
  else
    let c : 'a cell = init' v in 
    Wrap {head = c; last = c; sigma = sigma} 
[@@inline]


let return ?sigma v : 'a t = init ?sigma v 

let is_empty wrap = 
  match wrap with 
  | Empty _ -> true 
  | Wrap _ -> false


let is_not_leaf h l =
    let xj = (get_length h) - (get_length (get_jump h)) in
    let rj = (get_length (get_next l)) - (get_length (get_jump (get_next l))) in
    (not (get_length h = 0)) && (xj = 1 || xj = rj)

let is_leaf hn l = not (is_not_leaf hn l)

let cons v wrap = 
  match wrap with 
  | Empty s -> init ~sigma:s v
  | Wrap {head = h; last = l; sigma = s} ->
      let h_len = get_length h in 
      let h_hj = h_len - (get_length (get_jump h)) in 
      
      let condition = 
        h_hj = 1 || h_hj = (get_length (get_next l)) - (get_length (get_jump (get_next l))) 
      in 
      
      let c_jump = if condition then get_next l else l in 
      
      let c_height = if is_leaf h l then 0 else (get_height h) + 1 in 
      
      let c = 
        if c_height mod s = 0 then 
          let new_uncle = 
            if c_height <> 0 then get_jump l else get_jump (get_jump c_jump)
          in
          Skip {next = h; jump = c_jump; length = h_len + 1; value = v; height = c_height; shortcut = new_uncle}
        else 
          Normal {next = h; jump = c_jump; length = h_len + 1; value = v; height = c_height}
      in 
      
      let new_last = if condition then get_jump l else c 
    in Wrap {head = c; last = new_last; sigma = s}


let rec lookup c l =   
  match c with
  | Normal { next; jump; length=len; height=hei; _ } ->
      if len = l then 
        c
      else if hei > 0 && len - (1 lsl hei) >= l then 
        lookup jump l
      else 
        lookup next l

  | Skip { next; jump; shortcut; length=len; height=hei; _ } ->
      if len = l then 
        c
      else if hei > 0 then
        let jump_dist = 1 lsl hei in
        if len - (jump_dist lsl 1) + 2 >= l then 
          lookup shortcut l
        else if len - jump_dist >= l then 
          lookup jump l
        else 
          lookup next l
      else
        if get_length shortcut >= l then lookup shortcut l
        else if get_length jump >= l then lookup jump l
        else lookup next l

let get_exn wrap i = 
  match wrap with 
  | Empty _ -> invalid_arg "Empty List"
  | Wrap {head = h; last = _} -> 
    let h_len = get_length h in 
    if i < 0 || i > h_len then 
      invalid_arg "Invalid Index"
    else 
      let target = h_len - i in 
      get_value (lookup h target)

let get wrap i = try Some (get_exn wrap i) with Invalid_argument _ -> None

let index (wrap : 'a t) (i : int) : 'a cell = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> 
    let lenh = get_length h in 
    if i < 0 || i > lenh then 
      invalid_arg "Invalid Index"
    else 
      lookup h (lenh - i)

[@@inline]


let add_list wrap l = List.fold_left (fun acc v -> cons v acc) wrap (List.rev l)  



let rec rec_idx (n : int) : int=
	if n = 1 then 0
	else if n = 2 then 0
	else
		let rec largest_tree k = 
			if (1 lsl (k+1)) - 1 <= n then 
	  			largest_tree (k+1) else k 
		in
		let k = largest_tree 0 in
		let s = (1 lsl k) - 1 in
		let r = n - s in

		if r = 0 then n - 1
		else if r = s then s - 1
		else rec_idx r


let last_idx (n : int) : int = 
	if n = 1 then 0
	else if n = 2 then 1 
	else rec_idx n


let get_last_index (n : int) (i : int) : int = i + last_idx (n - i)


let set wrap i v =
	match wrap with
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _; sigma = s} ->
    let lenh = get_length h in 
		if i < 0 || i > lenh then 
			invalid_arg "Invalid Index"
		else
			let rec path acc c =
      let lenc = get_length c in 
			if lenc = (lenh - i) then begin
		  		if lenc = 0 then
					List.fold_left (fun w_acc x -> cons x w_acc) (empty_with_sigma s) (v :: acc)
		  		else
					let last_index = get_last_index (lenh + 1) (i + 1) in
					let l = index wrap last_index in
					let w = Wrap { head = (get_next c); last = l; sigma = s} in
					List.fold_left (fun w_acc x -> cons x w_acc) w (v :: acc)
			end
			else 
				path ((get_value c) :: acc) (get_next c)
			in path [] h



let hd wrap = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> get_value h


  let last wrap = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> get_exn wrap (get_length h)


let tl wrap = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _; sigma = s} ->
    let lenh = get_length h in 
		if lenh = 0 then 
			empty_with_sigma s
		else
	  		let last_index = get_last_index (lenh + 1) (lenh - 1) in 
	  		let l = index wrap last_index in
	  		Wrap {head = (get_next h); last = l; sigma = s}


let length wrap = 
	match wrap with 
	| Empty _ -> 0 
	| Wrap {head = h; last = _} -> (get_length h) + 1


let front wrap = 
	match wrap with 
	| Empty _ -> None
	| Wrap {head = h; last = _; sigma = s} ->
    let lenh = get_length h in 
		if lenh = 0 then 
			Some ((get_value h), empty_with_sigma s)
		else
	  		let last_index = get_last_index (lenh + 1) (lenh - 1) in 
	  		let l = index wrap last_index in
	  		Some ((get_value h), Wrap {head = (get_next h); last = l; sigma = s})


let front_exn wrap = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _; sigma = s} ->
    let lenh = get_length h in 
		if lenh = 0 then 
			(get_value h, empty_with_sigma s)
		else
	  		let last_index = get_last_index (lenh + 1) (lenh - 1) in 
	  		let l = index wrap last_index in
	  		(get_value h, Wrap {head = get_next h; last = l; sigma = s})



let get_and_remove_exn wrap i = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _; sigma = s} ->
    let lenh = get_length h in 
		if i < 0 || i > lenh then 
			invalid_arg "Invalid Index"
		else 
	  	let rec path acc c =
        let lenc = get_length c in 
			  if lenc = lenh - i then begin
		  		if lenc = 0 then
					(get_value c, List.fold_left (fun w_acc x -> cons x w_acc) (empty_with_sigma s) acc)
		  		else 
					let last_index = get_last_index (lenh + 1) (i + 1) in 
					let l = index wrap last_index in 
					let w = Wrap {head = get_next c; last = l; sigma = s} in 
					(get_value c, List.fold_left (fun w_acc x -> cons x w_acc) w acc)
			end 
			else 
				path ((get_value c) :: acc) (get_next c)
	  		in path [] h 




let remove wrap i = 
	match wrap with 
	| Empty _ -> invalid_arg "Empty List"
	| Wrap {head = h; last = _; sigma = s} ->
    let lenh = get_length h in 
		if i < 0 || i > lenh then 
			invalid_arg "Invalid Index"
		else 
	  	let rec path acc c =
        let lenc = get_length c in 
			  if lenc = lenh - i then begin
		 		if lenc = 0 then
					List.fold_left (fun w_acc x -> cons x w_acc) (empty_with_sigma s) acc
		  		else 
					let last_index = get_last_index (lenh + 1) (i + 1) in 
					let l = index wrap last_index in 
					let w = Wrap {head = get_next c; last = l; sigma = s} in 
					List.fold_left (fun w_acc x -> cons x w_acc) w acc
			end 
			else 
				path ((get_value c) :: acc) (get_next c)
	  		in path [] h 
	

let fold ~f ~x wrap = 
	match wrap with 
	| Empty _ -> x 
	| Wrap {head = h; last = _} -> 
		let rec aux acc c = 
			if get_length c = 0 then 
				f acc (get_value c)
			else 
				aux (f acc (get_value c)) (get_next c)
	  	in aux x h 


let to_list_rev wrap = fold ~x:[] wrap ~f:(fun acc x -> x :: acc)


let fold_rev ~f ~x wrap = List.fold_left f x (to_list_rev wrap)


let to_list wrap = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] wrap



let rev wrap = fold ~f:(fun acc x -> cons x acc) ~x:(empty_with_sigma (get_sigma wrap)) wrap


let map ~f wrap = fold_rev ~x:(empty_with_sigma (get_sigma wrap)) wrap ~f:(fun acc x -> cons (f x) acc)



let to_list_mapi_rev wrap f = 
	match wrap with 
	| Empty _-> [] 
	| Wrap {head = h; last = _} -> 
    let lenh = get_length h in 
		let rec aux acc c = 
        let lenc = get_length c in 
	  		if lenc  = 0 then 
				  ((f (lenh - lenc ) (get_value c)) :: acc)
	  		else
				  aux ((f (lenh - lenc ) (get_value c)) :: acc) (get_next c)
		in aux [] h 


let of_list ?(sigma = 2) l = add_list (empty_with_sigma sigma) l


let mapi ~f wrap = 
	match wrap with 
	| Empty s -> empty_with_sigma s 
	| Wrap _ -> 
		let l = to_list_mapi_rev wrap f in 
		of_list ~sigma:(get_sigma wrap) (List.rev l)


let iter ~f wrap = fold ~f:(fun () x -> f x) ~x:() wrap


let iteri ~f wrap = 
	match wrap with 
  	| Empty _-> () 
  	| Wrap {head = h; last = _} -> 
    let lenh = get_length h in 
		let rec aux c = 
        let lenc = get_length c in
	  		if lenc = 0 then 
				(f (lenh - lenc) (get_value c))
	  		else begin
				(f (lenh - lenc) (get_value c)); 
				aux (get_next c)
			end
		in aux h 


let rev_map ~f wrap = fold ~f:(fun acc x -> cons (f x) acc) ~x:(empty_with_sigma (get_sigma wrap)) wrap


let append l0 l1 = fold_rev ~f:(fun acc x -> cons x acc) ~x:l1 l0


let filter ~f wrap = fold_rev ~f:(fun acc x -> if f x then cons x acc else acc) ~x:(empty_with_sigma (get_sigma wrap)) wrap


let filter_map ~f wrap = 
	match wrap with
  | Empty s-> empty_with_sigma s
	| Wrap _ ->
		let func acc v = 
	  		match f v with 
	  		| Some x -> x :: acc
	  		| None -> acc
		in  
		let list = fold ~f:func ~x:[] wrap in of_list ~sigma:(get_sigma wrap) (List.rev list)


let flat_map f wrap = 
    fold_rev ~f:(fun acc v -> append (f v) acc) ~x:(empty_with_sigma (get_sigma wrap)) wrap


let flatten wrap = fold_rev ~f:(fun acc l -> append l acc) ~x:(empty_with_sigma (get_sigma wrap)) wrap


let app funs wrap =
	fold_rev ~f:(fun acc f -> fold_rev ~f:(fun acc x -> cons (f x) acc) ~x:acc wrap) ~x:(empty_with_sigma (get_sigma wrap)) funs


let take n wrap = 
	match wrap with 
	| Empty s -> empty_with_sigma s
	| Wrap {head = h; last = _; sigma = s} -> 
		if n > (get_length h) then 
			wrap
		else if n < 0 then 
			empty_with_sigma s
		else 
	  		let rec aux acc c i =
				if i >= n then
		 			of_list ~sigma:s (List.rev acc)
				else if get_length c = 0 then 
		  		of_list ~sigma:s (List.rev ((get_value c) :: acc)) 
				else 
					aux ((get_value c) :: acc) (get_next c) (i + 1)  
			in aux [] h 0


let take_while ~f wrap = 
	match wrap with 
	| Empty s -> empty_with_sigma s
	| Wrap {head = h; last = _; sigma = s} -> 
		let rec aux acc c = 
			if (f (get_value c)) then 
		  		if get_length c = 0 then 
					  of_list ~sigma:s (List.rev ((get_value c) :: acc))
		  		else 
					  aux ((get_value c) :: acc) (get_next c)
			else 
				of_list ~sigma:s (List.rev acc) 
		in aux [] h


let drop n wrap = 
	match wrap with 
  	| Empty s -> empty_with_sigma s
  	| Wrap {head = h; last = _; sigma = s} -> 
    let lenh = get_length h in 
		if n < 0 || n > lenh + 1 then 
			invalid_arg "Invalid Argument"
		else if n = lenh + 1 then 
			empty_with_sigma s
		else if n = 0 then 
			wrap
		else 
	  		let new_head = index wrap n in 
			let last_index = get_last_index (lenh + 1) n in 
			let new_last = index wrap last_index in 
			Wrap {head = new_head; last = new_last; sigma = s}


let drop_while ~f wrap =
	match wrap with
	| Empty s-> empty_with_sigma s
	| Wrap {head = h; last = _} ->
		let rec count n c =
			if f (get_value c) then
				if (get_length c) = 0 then 
					n + 1
				else 
					count (n + 1) (get_next c)
	  		else 
				n
		in drop (count 0 h) wrap


let take_drop n wrap = (take n wrap, drop n wrap)


let equal ~eq w1 w2 = 
	match w1, w2 with 
	| Empty s1, Empty s2 -> s1 = s2
	| Empty _, Wrap _ -> false
	| Wrap _, Empty _ -> false
  | Wrap {sigma=s1;_}, Wrap {sigma=s2;_} when s1 <> s2 -> false
	| Wrap {head = h1; last = _}, Wrap {head = h2; last = _} ->
		if get_length h1 <> get_length h2 then 
			false 
		else 
	  		let rec aux n c1 c2 = 
				if not (eq (get_value c1) (get_value c2)) then 
					false 
				else if n = 0 then 
					true
				else 
					aux (n - 1) (get_next c1) (get_next c2)
	  		in aux (get_length h1) h1 h2 


let make ?(sigma = 2) n v = 
	let rec aux n acc v = 
		if n <= 0 then 
			acc 
		else 
			aux (n-1) (cons v acc) v
  	in aux n (empty_with_sigma sigma) v



let repeat ?(sigma = 2) n wrap =
	if n < 0 then 
		invalid_arg "Invalid Argument"
	else 
		let rec aux n acc = 
			if n = 0 then 
				acc 
			else
				aux (n - 1) (append wrap acc)
		in aux n (empty_with_sigma sigma)


let range ?(sigma = 2) i j = 
	let rec aux i j acc = 
		if i = j then 
			cons i acc 
		else if i < j then 
			aux i (j-1) (cons j acc) 
		else 
			aux i (j+1) (cons j acc)
  	in aux i j (empty_with_sigma sigma) 


let range_r_open_ ?(sigma = 2) i j =
	if i=j then 
		empty_with_sigma sigma
	else if i<j then 
		range ~sigma i (j-1)
	else 
		range ~sigma i (j+1)


type 'a iter = ('a -> unit) -> unit


type 'a gen = unit -> 'a option


let add_list_map wrap l f = List.fold_left (fun acc v -> cons (f v) acc) wrap (List.rev l)


let to_list_map wrap f = fold_rev ~f:(fun acc v -> (f v) :: acc) ~x:[] wrap


let of_list_map ?(sigma = 2) ~f l = add_list_map (empty_with_sigma sigma) l f


let add_array wrap arr = Array.fold_right (fun v acc -> cons v acc) arr wrap
 

let of_array ?(sigma= 2) arr = add_array (empty_with_sigma sigma) arr


let to_array wrap = 
	match wrap with 
	| Empty _ -> [||]
	| Wrap {head = h; last = _} -> 
		let a = Array.make ((get_length h) + 1) (get_value h) in 
		let rec fill i c = 
	  		if (get_length c) = 0 then 
				Array.unsafe_set a i (get_value c)
	  		else begin
				Array.unsafe_set a i (get_value c); 
				fill (i+1) (get_next c)
	  		end
		in fill 0 h; a
 

let add_iter wrap s = 
	let l1 = ref (empty_with_sigma (get_sigma wrap)) in s (fun x -> l1 := cons x !l1); 
	fold ~f:(fun acc x -> cons x acc) ~x:wrap !l1


let of_iter ?(sigma = 2) s =  add_iter (empty_with_sigma sigma) s 


let to_iter wrap =  fun f -> iter ~f wrap


let rec gen_iter_ f g = 
	match g() with 
	| None -> ()
  	| Some x -> f x; gen_iter_ f g


let add_gen wrap g = 
  	let w1 = ref (empty_with_sigma (get_sigma wrap)) in 
  	gen_iter_  (fun x -> w1 := cons x !w1) g; 
  	fold ~f:(fun acc x -> cons x acc) ~x:wrap !w1


let of_gen ?(sigma=2) g = add_gen (empty_with_sigma sigma) g 


let to_gen wrap = 
  	match wrap with 
  	| Empty _ -> (fun () -> None)
  	| Wrap {head = h; last = _} -> 
		let curr = ref h in 
		let flag = ref false in 
		let next () = 
	  		if !flag then 
				None 
	  		else begin 
				let va = get_value !curr in 
				if get_length !curr = 0 then 
		  			flag := true
				else curr := get_next !curr;
					Some va 
	 		end
		in next



let compare ~cmp w1 w2 =
  	match w1, w2 with 
  	| Empty _ , Empty _ -> 0
  	| Empty _, Wrap _ -> -1 
  	| Wrap _, Empty _-> 1 
  	| Wrap {head = h1; last = _}, Wrap {head = h2; last = _} -> 
		let rec aux c1 c2 =
	  	let res = cmp (get_value c1) (get_value c2) in 
	  		if res <> 0 then 
				res 
	  		else
				match get_length c1 = 0, get_length c2 = 0 with 
				| true, true -> 0
				| false, true -> 1 
				| true, false -> -1 
				| false, false -> aux (get_next c1) (get_next c2)
		in aux h1 h2


module Infix = struct
	let (@+) = cons
	let (>>=) l f = flat_map f l
	let (>|=) l f = map ~f l
	let (<*>) = app
	let (--) = range
	let (--^) = range_r_open_
end

include Infix


type 'a printer = Format.formatter -> 'a -> unit


let pp ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_item fmt l =
    let first = ref true in
    iter ~f:(fun x ->
        if !first then
            first := false
        else
            pp_sep fmt ();
        pp_item fmt x) l;
    ()


