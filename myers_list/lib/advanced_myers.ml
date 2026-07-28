(** Additional Pointers: uncle e/ou inc *)
type 'a add_points = 
    | Normal 
    | Skip of 'a cell 
    | Leaf of 'a cell * 'a cell
and 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a; rhd : int; red : int; more : 'a add_points }

type 'a wrap = { head : 'a cell; last : 'a cell; sigma : int }

type 'a t = Nil | Wrap of 'a wrap

let init (v: 'a) : 'a cell =
    let rec c : 'a cell = { next = c; jump = c; more = Leaf (c, c) ; length = 1; value = v; rhd = -1; red = 0 } in
    c

(* Helper Functions *)

let pow2 n =
    if n >= 0 then
        1 lsl n
    else
        invalid_arg "pow2: n must be zero or positive"
[@@inline]

(** Hacker's Delight Second Edition p106 *)
let floor_log2 i = if i <= 0 then failwith "floor_log2: invalid input" else Sys.int_size - 1 - Ocaml_intrinsics_kernel.Int.count_leading_zeros i
[@@inline]

let k len = floor_log2 len
[@@inline]

let is_not_leaf h l =
    let xj = h.length - h.jump.length in
    let rj = l.next.length - l.next.jump.length in
    (not (h.length = 1)) && (xj = 1 || xj = rj)

let is_leaf h l = not (is_not_leaf h l)
[@@inline]

let calc_red (next : 'a cell) (leaf : bool) : int =  
  let len = next.length + 1 in
  match len with 
  | 1 -> 0
  | _ -> 
    match leaf with 
    | true -> k len 
    | false -> next.red - 1 
[@@inline]

let calc_rhd (c : 'a cell) (leaf : bool) : int = 
  if c.next.rhd < 0 then 
    (k c.length) - 1
  else
    if leaf && (match c.next.more with | Leaf _ -> true | _ -> false) then 
      0 
    else if not leaf then 
      c.jump.rhd - 1
    else 
      c.red - c.next.red
[@@inline]
   
let rec find_inc c rhd = match c.more with 
    | Leaf _ ->
        if c.rhd = rhd then
            Some c
        else if c.length = 1 then
            None
        else
            find_inc c.next rhd
    | _ ->
        let delta = rhd - c.rhd in
        let height = (k c.length) - c.red in
        if delta <> height then
            find_inc c.next rhd
        else
            find_inc c.jump rhd

let rec find_unc c red kn =
    if c.red = red then
        Some c
    else
        let temp = match c.more with 
        | Leaf (u,_) -> if u.red = red then Some u else None
        | Skip u -> if u.red = red then Some u else None
        | Normal -> None
        in
        match temp with
        | Some u -> temp
        | None ->
            if k c.length < kn then
                None
            else
                find_unc c.next red kn

let target_uncle_red src dst =
    if (k src) <> (k dst) then
        0
    else
        let rec descent s d depth =
            let ks = k s in
            let diff = (pow2 ks) - 1 in
            let s' = s - diff in
            let d' = d - diff in
            let ks' = k s' in
            if ks' <> (k d') then
                depth + (ks - ks')
            else
                descent s' d' (depth + (ks - ks'))
        in
        descent src dst 1

let is_parent src dst =
    src.length >= dst && dst >= src.length - (pow2 ((((k src.length) - src.red) + 1))) + 2
[@@inline]

(* Essential Functions *)

let cons (v: 'a) (list: 'a t) =
    match list with 
    | Nil -> 
        let c = init v in
        Wrap { head = c; last = c; sigma = 2 } (* maybe remove hardcoded default sigma *)
    | Wrap {head = h; last = l; sigma = s} -> 
        (* 1. define next and jump *)
        let length: int = h.length + 1 in
        let leaf: bool = is_leaf h l in
        let next: 'a cell = h in

        let jump: 'a cell = 
            let h_hj = h.length - h.jump.length in
		    if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then 
	  		    l.next
	  	    else 
	  		    l 
        in 

        (* 2. calculate red and rhd *)
        let red: int = calc_red next leaf in 
        let temp: 'a cell =  { next = h; jump = jump; more = Normal; length = length; value = v; rhd = -1; red = red } in 
        let temp_rhd = calc_rhd temp leaf in
        let rhd: int = if temp_rhd < 0 then -1 else temp_rhd in

        (* 3. determine if this node is normal, skip or leaf *)
        let skip: bool = (((k length) - red) mod s = 0) in

        let more = 
            if leaf then 
                (* 4. find inc *)
                let inc = find_inc h (rhd + 1) in
                (* 5. find uncle *)
                let unc = find_unc h rhd (k length) in
                Leaf (
                    (match unc with Some i -> i | None -> next), 
                    (match inc with Some i -> i | None -> next)
                )
            else if skip then
                (* 5. find uncle *)
                let unc = find_unc l.jump rhd (k length) in
                Skip (match unc with Some i -> i | None -> next)
            else 
                Normal in
        
        let c = { next = h; jump = jump; more = more; length = length; value = v; rhd = rhd; red = red } in
        let last: 'a cell = 
		    if leaf then 
	  		    c
	  	    else 
                l.jump
        in
        Wrap { head = c; last = last; sigma = s }

let rec lookup_descent cell len =
    if cell.length = len then
        cell
    (*
    else if cell.length = 1 then
        failwith "Unable to find cell"
    *)
    else if cell.jump.length >= len then
        lookup_descent cell.jump len
    else
        lookup_descent cell.next len
[@@inline]

let rec lookup_leaf cell len d sigma =
    match cell.more with
    | Leaf (u,i) ->
        if cell.rhd = d then
            lookup_descent u len
        else if cell.next.red = d then 
            lookup_descent cell.next len
        else if cell.jump.next.red = d then
            lookup_descent cell.jump.next len
        else
            lookup_leaf i len d sigma
    | _ -> failwith "Impossible state: lookup_leaf"

let rec lookup_uncle cell len d sigma =
    let find_branch c =
        let delta = d - c.rhd in
        let height = (k c.length) - c.red in
        if (delta >= 0) && (delta > height || (height - delta) mod sigma = 0) then
            lookup_uncle cell.jump len d sigma
        else
            lookup_uncle cell.next len d sigma
        in
    
    match cell.more with
    | Normal -> find_branch cell
    | Skip u -> 
        if cell.rhd = d then
            lookup_descent u len
        else
            find_branch cell
    | Leaf _ -> lookup_leaf cell len d sigma
    
let rec lookup_cell list len sigma = 
    (* Verify 4 cases *)
    if is_parent list len then
        (* Case 1: Descent *)
        lookup_descent list len
    else
        let d = target_uncle_red list.length len in
        match list.more with 
        | Leaf _ -> 
            if list.rhd > d then
                (* Case 4: Next *)
                lookup_cell list.next len sigma
            else
                lookup_uncle list len d sigma
        | _ ->
            (* Case 2: Search Skip *)
            (* Case 3: Search Leafs *)
            lookup_uncle list len d sigma

let rec lookup_wrap list len sigma last = 
    if len >= last.length then
        lookup_descent list len
    else
        let d = target_uncle_red list.length len in
        match list.more with 
        | Leaf _ -> 
            if list.rhd > d then
                lookup_cell list.next len sigma
            else
                lookup_uncle list len d sigma
        | _ ->
            lookup_uncle list len d sigma

let index l i = lookup_wrap l.head (l.head.length - i) l.sigma l.last

let lookup_t list len = match list with
    | Nil -> failwith "TODO"    (* TODO *)  
    | Wrap w -> (lookup_wrap w.head len w.sigma w.last).length

(* Helper Library Functions *)

let wrap c s =
    let rec find_last c =
        if c.red = k c.length || c.length = 1 then
            c
        else
            find_last c.jump
        in
    Wrap {head = c; last = find_last c; sigma = s}

(* Library Functions *)

let empty = Nil

let return x = 
    let c = init x in
    Wrap { head = c; last = c; sigma = 2 }

let return_sigma x s = 
    let c = init x in
    Wrap { head = c; last = c; sigma = s }

let is_empty = function
    | Nil -> true
    | Wrap _ -> false

let hd = function
    | Nil -> invalid_arg "Empty List"
    | Wrap c -> c.head.value

let tl = function
    | Nil -> invalid_arg "Empty List"
| Wrap {head = c; sigma = s; _} ->
        if c.length = 1 then
            Nil
        else
            wrap c.next s

let last = function
    | Nil -> invalid_arg "Empty List"
    | Wrap c -> 
        let rec go c =
            if c.length = 1 then
                c.value
            else
                go c.jump
        in
        go c.head

let front = function
    | Nil -> None
    | Wrap {head = c; sigma = s; _} -> 
        Some (c.value, wrap c.next s)

let front_exn = function
    | Nil -> invalid_arg "Empty List"
    | Wrap {head = c; sigma = s; _} -> 
        (c.value, wrap c.next s)

let length = function
    | Nil -> 0
    | Wrap c -> c.head.length

let get l i = match l with
    | Nil -> None
    | Wrap c -> 
        if i > c.head.length || i < 0 then
            None
        else
            Some (index c i).value

let get_exn l i = match l with
    | Nil -> invalid_arg "Empty List"
    | Wrap c -> 
        if i > c.head.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            (index c i).value

let cons' xs x = cons x xs

let fold ~f ~x wrap = 
	match wrap with 
	| Nil -> x 
	| Wrap {head = h; _} -> 
		let rec aux acc c = 
			if c.length = 1 then 
				f acc c.value 
			else 
				aux (f acc c.value) c.next 
	  	in aux x h 

let fold_rev ~f ~x wrap = 
	match wrap with 
	| Nil -> x 
	| Wrap {head = h; _} -> 
		let rec aux fnc acc c = 
			if c.length = 1 then 
				fnc acc c.value 
			else 
                let acc' = aux fnc acc c.next in
				fnc acc' c.value 
	  	in aux f x h 

let map ~f l =
    let func acc x = cons (f x) acc in
    fold_rev ~f:func ~x:Nil l

let mapi ~f l = match l with
    | Nil -> Nil
    | Wrap {head = c; _} -> 
        let rec aux i f acc c = match c with
            | {length = 1} -> return (f i c.value)
            | _ -> 
                let acc' = aux (i+1) f acc c.next in
                cons (f i c.value) acc'
        in
        aux 0 f Nil c

let set l i v = match l with
    | Nil -> invalid_arg "Empty List"
    | Wrap {head = c; sigma = s; _} ->
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            let rec aux len v acc curr =
                if curr.length = len then
                    if len = 1 then
                        return v
                    else
                        cons v (wrap curr.next s)
                else
                    let acc' = aux len v acc curr.next in
                    cons curr.value acc'
            in
            aux (c.length - i) v Nil c

let remove l i = match l with
    | Nil -> invalid_arg "Empty List"
| Wrap {head = c; sigma = s; _} ->
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            let rec aux len acc curr =
                if curr.length = len then
                    if len = 1 then
                        Nil
                    else
                        wrap curr.next s
                else
                    let acc' = aux len acc curr.next in
                    cons curr.value acc'
            in
            aux (c.length - i) Nil c

let get_and_remove_exn l i = match l with
    | Nil -> invalid_arg "Empty List"
| Wrap {head = c; sigma = s; _} ->
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            let rec aux len acc curr =
                if curr.length = len then
                    if len = 1 then
                        (curr.value, Nil)
                    else
                        (curr.value, wrap curr.next s)
                else
                    let (v, acc') = aux len acc curr.next in
                    (v, cons curr.value acc')
            in
            aux (c.length - i) Nil c

let append l1 l2 = fold_rev ~f:cons' ~x:l2 l1

let filter ~f l = 
    let func acc x = if (f x) then (cons x acc) else acc in
    fold_rev ~f:func ~x:Nil l

let filter_map ~f l = 
    let func acc x = match (f x) with
        | Some y -> (cons y acc)
        | None -> acc in
    fold_rev ~f:func ~x:Nil l

let flat_map f l =
    let func acc x = append (f x) acc in
    fold_rev ~f:func ~x:Nil l

let flatten lists = fold_rev ~f:(fun acc l -> append l acc) ~x:Nil lists

let app funs l =
    let func acc f = fold_rev ~f:(fun acc x -> cons (f x) acc) ~x:acc l in
    fold_rev ~f:func ~x:Nil funs

let rec take_ n l acc = match l with
    | Nil -> Nil
    | Wrap {head = c; sigma = s ; _} -> 
        if n = 0 then
            acc
        else if c.length = 1 then
            cons c.value acc
        else
            let acc' = take_ (n - 1) (wrap c.next s) acc in
            cons c.value acc'

let rec take n l = 
    if n < 0 then
        Nil
    else
        take_ n l Nil

let rec take_while_ f l acc = match l with
    | Nil -> Nil
    | Wrap {head = c; sigma = s; _} -> 
        if not (f c.value) then
            acc
        else if c.length = 1 then
            cons c.value acc
        else
            let acc' = take_while_ f (wrap c.next s) acc in
            cons c.value acc'

let rec take_while ~f l = take_while_ f l Nil

let rec drop n l = match l with
    | Nil ->
        if n > 1 || n < 0 then
            invalid_arg "Invalid Argument"
        else 
            Nil
    | Wrap {head = c; sigma = s; _} ->    
        if n > (c.length + 1) || n < 0 then
            invalid_arg "Invalid Argument"
        else
            if n = 0 then 
                l
            else if c.length = 1 then
                Nil
            else
                drop (n-1) (wrap c.next s)

let rec drop_while ~f l = match l with
    | Nil -> Nil
    | Wrap {head = c; sigma = s; _} ->    
        if not (f c.value) then 
            l
        else if c.length = 1 then
            Nil
        else
            drop_while ~f (wrap c.next s)

let rec take_drop_ n l acc = match l with
    | Nil -> (Nil, l)
    | Wrap {head = c; sigma = s; _} -> 
        if n = 0 then
            (acc, l)
        else if c.length = 1 then
            (cons c.value acc, Nil)
        else
            let (acc', xs)  = take_drop_ (n - 1) (wrap c.next s) acc in
            (cons c.value acc', xs)

let rec take_drop n l = 
    if n < 0 then
        (Nil, l)
    else
        take_drop_ n l Nil

let rec iter ~f l = match l with
    | Nil -> ()
    | Wrap {head = c; sigma = s; _} -> match c with
        | {length = 1} -> f c.value
        | _ -> f c.value; iter ~f (wrap c.next s)

let rec iteri_ i f l = match l with
    | Nil -> ()
    | Wrap {head = c; sigma = s; _} -> match c with
        | {length = 1} -> f i c.value
        | _ -> f i c.value; iteri_ (i+1) f (wrap c.next s)

let iteri ~f l = iteri_ 0 f l

let rev_map ~f l =
    let func acc x = cons (f x) acc in
    fold ~f:func ~x:Nil l

let rev l = fold ~f:cons' ~x:Nil l

let equal ~eq l1 l2 = match (l1, l2) with
    | (Nil, Nil) -> true
    | (Nil, Wrap _)
    | (Wrap _, Nil) -> false
    | (Wrap {head = c1; _}, Wrap {head = c2; _}) ->
        if c1.length <> c2.length then
            false
        else
            let rec equal' eq c1 c2 =
                if not (eq c1.value c2.value) then
                    false
                else if c1.length = 1 then
                    true
                else
                    equal' eq c1.next c2.next in
            equal' eq c1 c2

let compare ~cmp l1 l2 = match (l1, l2) with
    | (Nil, Nil) -> 0
    | (Nil, Wrap _) -> 1
    | (Wrap _, Nil) -> -1
    | (Wrap {head = c1; _}, Wrap {head = c2; _}) ->
        if c1.length < c2.length then
            -1    
        else if c1.length > c2.length then
            1
        else
            let rec compare' comp c1 c2 =
                let res = cmp c1.value c2.value in
                if res <> 0 || c1.length = 1 then
                    res
                else
                    compare' cmp c1.next c2.next in
            compare' cmp c1 c2

let make n x =
    let rec aux n acc x =
        if n <= 0 then
            acc
        else
            aux (n - 1) (cons x acc) x in
    aux n Nil x

let repeat n l =
    if n < 0 then
        invalid_arg "Invalid Argument"
    else
        let rec aux n l acc =
            if n <= 0 then
                acc
            else
                aux (n - 1) l (append l acc) in
        aux n l Nil

let range i j =
    let rec aux i j acc =
        if i = j then
            cons i acc
        else if i < j then
            aux i (j - 1) (cons j acc)
        else
            aux i (j + 1) (cons j acc) in
    aux i j Nil

let range_excl_ i j =
    if i = j then
        Nil
    else if i < j then
        range i (j - 1)
    else
        range i (j + 1)


let add_list l1 l2 = List.fold_left cons' l1 (List.rev l2)

let of_list l = List.fold_left cons' Nil (List.rev l)

let to_list l = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] l

let of_list_map ~f l = List.fold_left (fun acc x -> cons (f x) acc) Nil (List.rev l)

let of_array a = Array.fold_right (fun x acc -> cons x acc) a Nil

let add_array l a = Array.fold_right (fun x acc -> cons x acc) a l

let to_array l = match l with
    | Nil ->  [||]
    | Wrap {head = c; sigma = s; _} -> 
        let a = Array.make (c.length) c.value in
        if c.length <> 1 then
            iteri ~f:(fun i x -> Array.set a (i+1) x) (wrap c.next s);
        a

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

let add_iter l it = 
    let res = ref Nil in
    it (fun x -> res := cons x !res);
    fold ~f:cons' ~x:l !res

let of_iter it = 
    let l = ref Nil in
    it (fun x -> l := cons x !l);
    rev !l

let to_iter l yield = iter ~f:yield l 

let add_gen l g =
    let rec gen_iter f g =
        match g () with
        | None -> ()
        | Some x ->
            f x;
            gen_iter f g in
    let res = ref Nil in
    gen_iter (fun x -> res := cons x !res) g;
    fold ~f:(fun acc x -> cons x acc) ~x:l !res

let of_gen g = add_gen Nil g

let to_gen l = match l with
    | Nil -> (fun () -> None)
    | Wrap {head = c; _} -> 
        let curr = ref c in 
        let flag = ref false in 
        let go () = 
            if !flag then 
                None 
            else 
                begin 
                let va = !curr.value in 
                if !curr.length = 1 then 
                    flag := true
                else 
                    curr := !curr.next;
                Some va 
                end in 
        go

module Infix = struct
    let ( @+ ) = cons
    let ( >>= ) l f = flat_map f l
    let ( >|= ) l f = map ~f l
    let ( <*> ) = app
    let ( -- ) = range
    let ( --^ ) = range_excl_
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
