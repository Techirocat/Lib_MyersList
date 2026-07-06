(** Titulo*)

type 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a }


type 'a wrap = { head : 'a cell; last : 'a cell }


type 'a t = 
	| Empty
	| Wrap of {head : 'a cell; last : 'a cell}


(** Algum titulo*)


let empty = Empty


let init' (v : 'a) : 'a cell =
	let rec c : 'a cell = { next = c; jump = c; length = 0; value = v } in c
[@@inline]


let init (v : 'a) : 'a t =
	let c : 'a cell = init' v in 
	Wrap { head = c; last = c }
[@@inline]


let return (v : 'a) : 'a t = init v


let is_empty wrap = 
	match wrap with
	| Empty -> true
	| Wrap _ -> false


let rec lookup (c : 'a cell) (l : int) : 'a cell =
	if c.length = l then 
		c
	else 
		let j = c.jump in
		let c' = if j.length < l then c.next else j in
		lookup c' l


let index (wrap : 'a t) (i : int) : 'a cell = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> lookup h (h.length - i)
[@@inline]


let get_exn wrap i= 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> 
		if i < 0 || i > h.length then 
			invalid_arg "Invalid Index"
		else 
	  		(lookup h (h.length - i)).value


let get wrap i = try Some (get_exn wrap i) with Invalid_argument _ -> None


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


let cons v wrap =
	match wrap with 
	| Empty -> init v
	| Wrap {head = h; last = l} -> 
		let h_len = h.length in
		let h_hj = h_len - h.jump.length in
		if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then 
	  		let c = { next = h; jump = l.next; length = h_len + 1; value = v } 
	  		in Wrap { head = c; last = l.jump }
		else 
	  		let c = { next = h; jump = l; length = h_len + 1; value = v } 
	  		in Wrap { head = c; last = c }
[@@inline]


let add_list wrap l = List.fold_left (fun acc v -> cons v acc) wrap (List.rev l)  


let set wrap i v =
	match wrap with
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} ->
		if i < 0 || i > h.length then 
			invalid_arg "Invalid Index"
		else
			let rec path acc c =
			if c.length = (h.length - i) then begin
		  		if c.length = 0 then
					List.fold_left (fun w_acc x -> cons x w_acc) empty (v :: acc)
		  		else
					let last_index = get_last_index (h.length + 1) (i + 1) in
					let l = index wrap last_index in
					let w = Wrap { head = c.next; last = l } in
					List.fold_left (fun w_acc x -> cons x w_acc) w (v :: acc)
			end
			else 
				path (c.value :: acc) c.next
			in path [] h


let hd wrap = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> h.value


let last wrap = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} -> get_exn wrap h.length


let tl wrap = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} ->
		if h.length = 0 then 
			empty
		else
	  		let last_index = get_last_index (h.length + 1) (h.length - 1) in 
	  		let l = index wrap last_index in
	  		Wrap {head = h.next; last = l}


let length wrap = 
	match wrap with 
	| Empty -> 0 
	| Wrap {head = h; last = _} -> h.length + 1


let front wrap = 
	match wrap with 
	| Empty -> None
	| Wrap {head = h; last = _} ->
		if h.length = 0 then 
			Some (h.value, empty)
		else
	  		let last_index = get_last_index (h.length + 1) (h.length - 1) in 
	  		let l = index wrap last_index in
	  		Some (h.value, Wrap {head = h.next; last = l})


let front_exn wrap = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} ->
		if h.length = 0 then 
			(h.value, empty)
		else
	  		let last_index = get_last_index (h.length + 1) (h.length - 1) in 
	  		let l = index wrap last_index in
	  		(h.value, Wrap {head = h.next; last = l})



let get_and_remove_exn wrap i = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} ->
		if i < 0 || i > h.length then 
			invalid_arg "Invalid Index"
		else 
	  		let rec path acc c =
			if c.length = (h.length - i) then begin
		  		if c.length = 0 then
					(c.value, List.fold_left (fun w_acc x -> cons x w_acc) empty acc)
		  		else 
					let last_index = get_last_index (h.length + 1) (i + 1) in 
					let l = index wrap last_index in 
					let w = Wrap {head = c.next; last = l} in 
					(c.value, List.fold_left (fun w_acc x -> cons x w_acc) w acc)
			end 
			else 
				path (c.value :: acc) c.next
	  		in path [] h 


let remove wrap i = 
	match wrap with 
	| Empty -> invalid_arg "Empty List"
	| Wrap {head = h; last = _} ->
		if i < 0 || i > h.length then 
			invalid_arg "Invalid Index"
		else 
	  		let rec path acc c =
			if c.length = (h.length - i) then begin
		 		if c.length = 0 then
					List.fold_left (fun w_acc x -> cons x w_acc) empty acc
		  		else 
					let last_index = get_last_index (h.length + 1) (i + 1) in 
					let l = index wrap last_index in 
					let w = Wrap {head = c.next; last = l} in 
					List.fold_left (fun w_acc x -> cons x w_acc) w acc
			end 
			else 
				path (c.value :: acc) c.next
	  		in path [] h 
	

let fold ~f ~x wrap = 
	match wrap with 
	| Empty -> x 
	| Wrap {head = h; last = _} -> 
		let rec aux acc c = 
			if c.length = 0 then 
				f acc c.value 
			else 
				aux (f acc c.value) c.next 
	  	in aux x h 


let to_list_rev wrap = fold ~x:[] wrap ~f:(fun acc x -> x :: acc)


let fold_rev ~f ~x wrap = List.fold_left f x (to_list_rev wrap)


let to_list wrap = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] wrap



let rev wrap = fold ~f:(fun acc x -> cons x acc) ~x:empty wrap


let map ~f wrap = fold_rev ~x:empty wrap ~f:(fun acc x -> cons (f x) acc)


let to_list_mapi_rev wrap f = 
	match wrap with 
	| Empty -> [] 
	| Wrap {head = h; last = _} -> 
		let rec aux acc c = 
	  		if c.length = 0 then 
				((f (h.length - c.length) c.value) :: acc)
	  		else
				aux ((f (h.length - c.length) c.value) :: acc) c.next
		in aux [] h 


let of_list l = add_list empty l


let mapi ~f wrap = 
	match wrap with 
	| Empty -> Empty 
	| Wrap _ -> 
		let l = to_list_mapi_rev wrap f in 
		of_list (List.rev l)


let iter ~f wrap = fold ~f:(fun () x -> f x) ~x:() wrap


let iteri ~f wrap = 
	match wrap with 
  	| Empty -> () 
  	| Wrap {head = h; last = _} -> 
		let rec aux c = 
	  		if c.length = 0 then 
				(f (h.length - c.length) c.value)
	  		else begin
				(f (h.length - c.length) c.value); 
				aux c.next 
			end
		in aux h 


let rev_map ~f wrap = fold ~f:(fun acc x -> cons (f x) acc) ~x:empty wrap


let append l0 l1 = fold_rev ~f:(fun acc x -> cons x acc) ~x:l1 l0


let filter ~f wrap = fold_rev ~f:(fun acc x -> if f x then cons x acc else acc) ~x:empty wrap


let filter_map ~f wrap = 
	match wrap with
   	| Empty -> Empty
	| Wrap _ ->
		let func acc v = 
	  		match f v with 
	  		| Some x -> x :: acc
	  		| None -> acc
		in  
		let list = fold ~f:func ~x:[] wrap in of_list (List.rev list)


let flat_map f wrap = fold ~f:(fun acc v -> append acc (f v)) ~x:empty wrap


let flatten wrap = fold_rev ~f:(fun acc l -> append l acc) ~x:empty wrap


let app funs wrap =
	fold_rev ~f:(fun acc f -> fold_rev ~f:(fun acc x -> cons (f x) acc) ~x:acc wrap) ~x:empty funs


let take n wrap = 
	match wrap with 
	| Empty -> Empty
	| Wrap {head = h; last = _} -> 
		if n > h.length then 
			wrap
		else if n < 0 then 
			Empty
		else 
	  		let rec aux acc c i =
				if i >= n then
		 			of_list (List.rev acc)
				else if c.length = 0 then 
		  			of_list (List.rev (c.value :: acc)) 
				else 
					aux (c.value :: acc) c.next (i + 1)  
			in aux [] h 0


let take_while ~f wrap = 
	match wrap with 
	| Empty -> Empty
	| Wrap {head = h; last = _} -> 
		let rec aux acc c = 
			if (f c.value) then 
		  		if c.length = 0 then 
					of_list (List.rev (c.value :: acc))
		  		else 
					aux (c.value :: acc) c.next
			else 
				of_list (List.rev acc) 
		in aux [] h


let drop n wrap = 
	match wrap with 
  	| Empty -> Empty
  	| Wrap {head = h; last = _} -> 
		if n < 0 || n > h.length + 1 then 
			invalid_arg "Invalid Argument"
		else if n = h.length + 1 then 
			empty
		else if n = 0 then 
			wrap
		else 
	  		let new_head = index wrap n in 
			let last_index = get_last_index (h.length + 1) n in 
			let new_last = index wrap last_index in 
			Wrap {head = new_head; last = new_last}


let drop_while ~f wrap =
	match wrap with
	| Empty -> Empty
	| Wrap {head = h; last = _} ->
		let rec count n c =
			if f c.value then
				if c.length = 0 then 
					n + 1
				else 
					count (n + 1) c.next
	  		else 
				n
		in drop (count 0 h) wrap


let take_drop n wrap = (take n wrap, drop n wrap)


let equal ~eq w1 w2 = 
	match w1, w2 with 
	| Empty, Empty -> true
	| Empty, Wrap _ -> false
	| Wrap _, Empty -> false
	| Wrap {head = h1; last = _}, Wrap {head = h2; last = _} ->
		if h1.length != h2.length then 
			false 
		else 
	  		let rec aux n c1 c2 = 
				if not (eq c1.value c2.value) then 
					false 
				else if n = 0 then 
					true
				else 
					aux (n - 1) c1.next c2.next
	  		in aux h1.length h1 h2 


let make n v = 
	let rec aux n acc v = 
		if n <= 0 then 
			acc 
		else 
			aux (n-1) (cons v acc) v
  	in aux n empty v



let repeat n wrap =
	if n < 0 then 
		invalid_arg "Invalid Argument"
	else 
		let rec aux n acc = 
			if n = 0 then 
				acc 
			else
				aux (n - 1) (append wrap acc)
		in aux n empty 


let range i j = 
	let rec aux i j acc = 
		if i = j then 
			cons i acc 
		else if i < j then 
			aux i (j-1) (cons j acc) 
		else 
			aux i (j+1) (cons j acc)
  	in aux i j empty 


let range_r_open_ i j =
	if i=j then 
		empty
  	else if i<j then 
		range i (j-1)
  	else 
		range i (j+1)


type 'a iter = ('a -> unit) -> unit


type 'a gen = unit -> 'a option


let add_list_map wrap l f = List.fold_left (fun acc v -> cons (f v) acc) wrap (List.rev l)


let to_list_map wrap f = fold_rev ~f:(fun acc v -> (f v) :: acc) ~x:[] wrap


let of_list_map ~f l = add_list_map empty l f


let add_array wrap arr = Array.fold_right (fun v acc -> cons v acc) arr wrap
 

let of_array arr = add_array empty arr


let to_array wrap = 
	match wrap with 
	| Empty -> [||]
	| Wrap {head = h; last = _} -> 
		let a = Array.make (h.length + 1) h.value in 
		let rec fill i c = 
	  		if c.length = 0 then 
				Array.unsafe_set a i c.value
	  		else begin
				Array.unsafe_set a i c.value; 
				fill (i+1) c.next
	  		end
		in fill 0 h; a
 

let add_iter wrap s = 
	let l1 = ref empty in s (fun x -> l1 := cons x !l1); 
	fold ~f:(fun acc x -> cons x acc) ~x:wrap !l1


let of_iter s =  add_iter empty s 


let to_iter wrap =  fun f -> iter ~f wrap


let rec gen_iter_ f g = 
	match g() with 
	| None -> ()
  	| Some x -> f x; gen_iter_ f g


let add_gen wrap g = 
  	let w1 = ref empty in 
  	gen_iter_  (fun x -> w1 := cons x !w1) g; 
  	fold ~f:(fun acc x -> cons x acc) ~x:wrap !w1


let of_gen g = add_gen empty g 


let to_gen wrap = 
  	match wrap with 
  	| Empty -> (fun () -> None)
  	| Wrap {head = h; last = _} -> 
		let curr = ref h in 
		let flag = ref false in 
		let next () = 
	  		if !flag then 
				None 
	  		else begin 
				let va = !curr.value in 
				if !curr.length = 0 then 
		  			flag := true
				else curr := !curr.next;
					Some va 
	 		end
		in next



let compare ~cmp w1 w2 =
  	match w1, w2 with 
  	| Empty, Empty -> 0
  	| Empty, Wrap _ -> -1 
  	| Wrap _, Empty -> 1 
  	| Wrap {head = h1; last = _}, Wrap {head = h2; last = _} -> 
		let rec aux c1 c2 =
	  	let res = cmp c1.value c2.value in 
	  		if res != 0 then 
				res 
	  		else
				match c1.length = 0, c2.length = 0 with 
				| true, true -> 0
				| false, true -> 1 
				| true, false -> -1 
				| false, false -> aux c1.next c2.next
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

