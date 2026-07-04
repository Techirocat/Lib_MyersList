(* 
MIT License
Copyright (c) 2025 Programming Language Innovation Lab @ NUS 
*)

(* Original: 3 floor(lg(n+1)) - 2 *)

type 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a }

(**************************************)
(* High-performance functions *)

let init (_skip : int) (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; jump = c; length = 0; value = v } in
  c
[@@inline]

let cons_ (_skip : int) (v : 'a) (c : 'a cell) : 'a cell =
  let j = c.jump in
  let j' = if c.length - j.length == j.length - j.jump.length then j.jump else c in
  { next = c; jump = j'; length = c.length + 1; value = v }
[@@inline]

let rec lookup (skip : int) (c : 'a cell) (l : int) : 'a cell =
  if c.length = l
  then c
  else let j = c.jump in
       let c' = if j.length < l then c.next else j in
       lookup skip c' l

let index (skip : int) (c : 'a cell) (i : int) : 'a cell = lookup skip c (c.length - i)
[@@inline]

let make_list_rev (skip : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (c : 'a cell) : 'a list -> 'a cell = function
  | [] -> c
  | v :: vs -> go (cons_ skip v c) vs in
  go (init skip v) vs

let make_tree_rev (skip : int) (height : int) (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (last : 'a cell) (last_len : int) (vs : 'a list) : int -> 'a cell * 'a cell * 'a list = function
  | 0 -> let v' :: vs' = vs
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = { next = last; jump = last; length = last_len + 1; value = v' } in
         (c, last, vs')
  | h -> let right, right_last, vs' = go last last_len vs (h - 1) in
         let left, _, v'' :: vs'' = go right (last_len + 1 lsl h - 1) vs' (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = { next = left; jump = right_last; length = last_len + 2 lsl h - 1; value = v'' } in
         (c, last, vs'') in
  let c, _, _ = go (init skip v) 0 vs (height - 1) in
  c

(**************************************)
(* Non-high-performance functions *)

let find_path (_skip : int) (c : 'a cell) (l : int) : 'a cell list =
  let rec go (c : 'a cell) (l : int) (p : 'a cell list) : 'a cell list =
    if c.length = l
    then p
    else let j = c.jump in
         let c' = if j.length < l then c.next else j in
         go c' l (c' :: p) in
  go c l [c]

let index_path (skip : int) (c : 'a cell) (i : int) : 'a cell list = find_path skip c (c.length - i)
[@@inline]

(**************************************)
(* Interface and generic functions *)
(*
module Implementation : Myers.Implementation with type 'a t = 'a cell = struct
  type 'a t = 'a cell
  type 'a t' = 'a cell

  let next (c : 'a cell) : 'a cell = c.next
  let jump (c : 'a cell) : 'a cell = c.jump
  let length (c : 'a cell) : int = c.length
  let car (c : 'a cell) : 'a = c.value
  let unwrap (c : 'a cell) : 'a cell = c

  let init = init
  let cons = cons
  let lookup = lookup
  let index = index

  let make_list_rev = make_list_rev
  let make_tree_rev = make_tree_rev

  let find_path = find_path
  let index_path = index_path
  let length_of_height _ h = 1 lsl h - 1
end

module Generic = Myers.Generic (Implementation)
*)

(**************************************)
(* Wrapper *)

type 'a t = Nil | Cell of 'a cell
let empty = Nil
let return x = Cell (init 0 x)

let is_empty = function
    | Nil -> false
    | Cell _ -> true

let hd = function
    | Nil -> invalid_arg "Empty List"
    | Cell {value} -> value

let tl = function
    | Nil -> invalid_arg "Empty List"
    | Cell {next} -> Cell next

let front = function
    | Nil -> None
    | Cell c -> Some (c.value, Cell c.next)

let front_exn = function
    | Nil -> invalid_arg "Empty List"
    | Cell c -> (c.value, Cell c.next) 

let length = function
    | Nil -> 0
    | Cell c -> c.length + 1

let get l i = match l with
    | Nil -> None
    | Cell c -> 
        if i > c.length || i < 0 then
            None
        else
            Some (index 0 c i).value

let get_exn l i = match l with
    | Nil -> invalid_arg "Empty List"
    | Cell c -> 
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            (index 0 c i).value

let cons x l = match l with
    | Nil -> return x
    | Cell c -> Cell (cons_ 0 x c)

let cons' xs x = cons x xs

let rec fold ~f ~x:acc l = match l with
    | Nil -> acc
    | Cell c -> match c with
        | {length = 0} -> f acc c.value
        | _ -> fold ~f ~x:(f acc c.value) (Cell (c.next))

let rec fold_rev ~f ~x:acc l = match l with
    | Nil -> acc
    | Cell c -> match c with
        | {length = 0} -> f acc c.value
        | _ -> 
            let acc' = fold_rev ~f ~x:acc (Cell (c.next)) in
            f acc' c.value

let map ~f l =
    let func acc x = cons (f x) acc in
    fold_rev ~f:func ~x:Nil l

(* TODO: tentar refazer com fold_rev *)
let mapi ~f l = match l with
    | Nil -> Nil
    | Cell c ->
        let rec go (idx: int) (acc: 'b list) (f: int -> 'a -> 'b) (curr: 'a cell) = match curr with 
            | {length = 0} -> (f idx curr.value) :: acc
            | _ -> go (idx + 1) ((f idx curr.value) :: acc) f curr.next in
        List.fold_left cons' Nil (go 0 [] f c)

let set l i v = match l with
    | Nil -> invalid_arg "Empty List"
    | Cell c ->
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            let rec go acc len curr =
                if curr.length = len then
                    (acc, curr.next)
                else
                    go (curr.value :: acc) len curr.next in
            let info = go [] (c.length - i) c in
            let info' = (v :: (fst info), snd info) in
            List.fold_left cons' (Cell (snd info')) (fst info')

let remove l i = match l with
    | Nil -> invalid_arg "Empty List"
    | Cell c ->
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            let rec go acc len curr =
                if curr.length = len then
                    (acc, curr.next)
                else
                    go (curr.value :: acc) len curr.next in
            let info = go [] (c.length - i) c in
            List.fold_left cons' (Cell (snd info)) (fst info)

let get_and_remove_exn l i = match l with
    | Nil -> invalid_arg "Empty List"
    | Cell c ->
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            let rec go acc len curr =
                if curr.length = len then
                    (acc, curr)
                else
                    go (curr.value :: acc) len curr.next in
            let info = go [] (c.length - i) c in
            ((snd info).value, List.fold_left cons' (Cell ((snd info).next)) (fst info))

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

(* TODO: refazer com append *)
let flatten lists = fold_rev ~f:(fun acc l -> fold_rev ~f:cons' ~x:acc l) ~x:Nil lists

let app funs l =
    let func acc f = fold_rev ~f:(fun acc x -> cons (f x) acc) ~x:acc l in
    fold_rev ~f:func ~x:Nil funs

let rec take_ n l acc = match l with
    | Nil -> Nil
    | Cell c -> 
        if n = 0 then
            acc     (* TODO: ver se ainda tenho q juntar o c.value *)
        else if c.length = 0 then
            cons c.value acc
        else
            let acc' = take_ (n - 1) (Cell (c.next)) acc in
            cons c.value acc'

let rec take n l = 
    if n < 0 then
        Nil
    else
        take_ n l Nil

let rec take_while_ f l acc = match l with
    | Nil -> Nil
    | Cell c -> 
        if not (f c.value) then
            acc     (* TODO: ver se ainda tenho q juntar o c.value *)
        else if c.length = 0 then
            cons c.value acc
        else
            let acc' = take_while_ f (Cell (c.next)) acc in
            cons c.value acc'

let rec take_while ~f l = take_while_ f l Nil

let rec drop n l = match l with
    | Nil -> Nil
    | Cell c ->    
        if n = 0 then 
            l
        else if c.length = 0 then
            Nil
        else
            drop (n-1) (Cell (c.next))

let rec drop_while ~f l = match l with
    | Nil -> Nil
    | Cell c ->    
        if not (f c.value) then 
            l
        else if c.length = 0 then
            Nil
        else
            drop_while ~f (Cell (c.next))

let rec take_drop_ n l acc = match l with
    | Nil -> (Nil, l)
    | Cell c -> 
        if n = 0 then
            (acc, l)     (* TODO: ver se ainda tenho q juntar o c.value *)
        else if c.length = 0 then
            (cons c.value acc, Nil)
        else
            let (acc', xs)  = take_drop_ (n - 1) (Cell (c.next)) acc in
            (cons c.value acc', xs)

let rec take_drop n l = 
    if n < 0 then
        (Nil, l)
    else
        take_drop_ n l Nil

let rec iter ~f l = match l with
    | Nil -> ()
    | Cell c -> match c with
        | {length = 0} -> f c.value
        | _ -> f c.value; iter ~f (Cell (c.next))

let rec iteri_ i f l = match l with
    | Nil -> ()
    | Cell c -> match c with
        | {length = 0} -> f i c.value
        | _ -> f i c.value; iteri_ (i+1) f (Cell (c.next))

let iteri ~f l = iteri_ 0 f l

let rev_map ~f l =
    let func acc x = cons (f x) acc in
    fold ~f:func ~x:Nil l

let rev l = fold ~f:cons' ~x:Nil l

let equal ~eq l1 l2 = match (l1, l2) with
    | (Nil, Nil) -> true
    | (Nil, Cell _)
    | (Cell _, Nil) -> false
    | (Cell c1, Cell c2) ->
        if c1.length <> c2.length then
            false
        else
            let rec equal' eq c1 c2 =
                if not (eq c1.value c2.value) then
                    false
                else if c1.length = 0 then
                    true
                else
                    equal' eq c1.next c2.next in
            equal' eq c1 c2

let compare ~cmp l1 l2 = match (l1, l2) with
    | (Nil, Nil) -> 0
    | (Nil, Cell _) -> 1
    | (Cell _, Nil) -> -1
    | (Cell c1, Cell c2) ->
        if c1.length < c2.length then
            -1    
        else if c1.length > c2.length then
            1
        else
            let rec compare' comp c1 c2 =
                let res = cmp c1.value c2.value in
                if res <> 0 || c1.length = 0 then
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


let add_list l1 l2 = List.fold_right cons l2 l1 (* TODO: talvez usar fold_left pois é TR *)

let of_list l = List.fold_right cons l Nil (* TODO: talvez usar fold_left pois é TR *)

let to_list l = fold_rev ~f:(fun acc x -> x :: acc) ~x:[] l

let of_list_map ~f l = List.fold_right (fun x acc -> cons (f x) acc) l Nil (* TODO: talvez usar fold_left pois é TR *)

let of_array a = Array.fold_right (fun x acc -> cons x acc) a Nil

let add_array l a = Array.fold_right (fun x acc -> cons x acc) a l

let to_array l = match l with
    | Nil ->  [||]
    | Cell c -> 
        let a = Array.make (c.length + 1) c.value in
        iteri ~f:(fun i x -> Array.set a i x) (Cell c.next);
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

(* TODO: rever função *)
let to_iter l yield = iter ~f:yield l 

(* TODO: rever função *)
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

(* TODO: rever função *)
let to_gen l = match l with
    | Nil -> (fun () -> None)
    | Cell c -> 
        let curr = ref c in 
        let flag = ref false in 
        let go () = 
            if !flag then 
                None 
            else 
                begin 
                let va = !curr.value in 
                if !curr.length = 0 then 
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

(* TODO: rever função *)
let pp ?(pp_sep = fun fmt () -> Format.fprintf fmt ",@ ") pp_item fmt l =
    let first = ref true in
    iter ~f:(fun x ->
        if !first then
            first := false
        else
            pp_sep fmt ();
        pp_item fmt x) l;
    ()
