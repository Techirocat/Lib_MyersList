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

let is_empty l = function
    | Nil -> false
    | Cell _ -> true

let hd l = function
    | Nil -> invalid_arg "Empty List"
    | Cell {value} -> value

let tl l = function
    | Nil -> invalid_arg "Empty List"
    | Cell {next} -> next

let front l = function
    | Nil -> None
    | Cell c -> Some (c.value, c.next)

let front_exn l = function
    | Nil -> invalid_arg "Empty List"
    | Cell c -> (c.value, c.next) 

let length l = l.length + 1

let get l i = match l with
    | Nil -> None
    | Cell c -> 
        if i > c.length || i < 0 then
            None
        else
            Some (index 0 c i)

let get_exn l i = match l with
    | Nil -> invalid_arg "Empty List"
    | Cell c -> 
        if i > c.length || i < 0 then
            invalid_arg "Invalid Index"
        else
            index 0 c i

let cons x l = match l with
    | Nil -> return x
    | Cell c -> Cell (cons_ 0 x c)

let cons' xs x = cons x xs

let rec fold f acc l = match l with
    | Nil -> acc
    | Cell c -> match c with
        | {length = 0} -> f acc c.value
        | _ -> fold f (f acc c.value) (Cell (c.next))

let rec fold_rev f acc l = match l with
    | Nil -> acc
    | Cell c -> match c with
        | {length = 0} -> f acc c.value
        | _ -> 
            let acc' = fold_rev f acc (Cell (c.next)) in
            f acc' c.value

(*
let map f l = match l with
    | Nil -> Nil
    | Cell c ->
        let rec go (acc: 'a list) (f: 'a -> 'b) (curr: 'a cell) = match curr with 
            | {length = 0} -> f curr.value :: acc
            | _ -> go (f curr.value :: acc) f curr.next in
        List.fold_left cons' Nil (go [] f c)
*)

let map f l =
    let func acc x = cons (f x) acc in
    fold_rev func Nil l

(* TODO: tentar refazer com fold_rev *)
let mapi i f l = match l with
    | Nil -> Nil
    | Cell c ->
        let rec go (idx: int) (acc: 'a list) (f: int -> 'a -> 'b) (curr: 'a cell) = match curr with 
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
                if curr.length == len then
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
                if curr.length == len then
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
                if curr.length == len then
                    (acc, curr)
                else
                    go (curr.value :: acc) len curr.next in
            let info = go [] (c.length - i) c in
            ((snd info).value, List.fold_left cons' (Cell ((snd info).next)) (fst info))

let append l1 l2 = fold_rev cons' l2 l1

let filter f l = 
    let func acc x = if (f x) then (cons x acc) else acc in
    fold_rev func Nil l

let filter_map f l = 
    let func acc x = match (f x) with
        | Some y -> (cons y acc)
        | None -> acc in
    fold_rev func Nil l

let flat_map f l =
    let func acc x = append (f x) acc in
    fold_rev func Nil l

(* TODO: refazer com append *)
let flatten lists =  fold_rev (fold_rev cons') Nil lists

let app funs l =
    let func acc f = fold_rev (fun acc x -> cons (f x) acc) acc l in
    fold_rev func Nil funs

let rec take_ n l acc = match l with
    | Nil -> Nil
    | Cell c -> 
        if n == 0 then
            acc     (* TODO: ver se ainda tenho q juntar o c.value *)
        else if c.length == 0 then
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
        if !(f c.value) then
            acc     (* TODO: ver se ainda tenho q juntar o c.value *)
        else if c.length == 0 then
            cons c.value acc
        else
            let acc' = take_while_ f (Cell (c.next)) acc in
            cons c.value acc'

let rec take_while f l = take_while_ f l Nil

let rec drop n l = match l with
    | Nil -> Nil
    | Cell c ->    
        if n == 0 then 
            l
        else if c.length == 0 then
            Nil
        else
            drop (n-1) (Cell (c.next))

let rec drop_while f l = match l with
    | Nil -> Nil
    | Cell c ->    
        if !(f c.value) then 
            l
        else if c.length == 0 then
            Nil
        else
            drop_while f (Cell (c.next))

let rec take_drop_ n l acc = match l with
    | Nil -> (Nil, l)
    | Cell c -> 
        if n == 0 then
            (acc, l)     (* TODO: ver se ainda tenho q juntar o c.value *)
        else if c.length == 0 then
            (cons c.value acc, Nil)
        else
            let (acc', xs)  = take_drop_ (n - 1) (Cell (c.next)) acc in
            (cons c.value acc', xs)

let rec take_drop n l = 
    if n < 0 then
        (Nil, l)
    else
        take_drop_ n l Nil

let rec iter f l = match l with
    | Nil -> ()
    | Cell c -> match c with
        | {length = 0} -> f c.value
        | _ -> f c.value; iter f (Cell (c.next))

let rec iteri_ i f l = match l with
    | Nil -> ()
    | Cell c -> match c with
        | {length = 0} -> f i c.value
        | _ -> f i c.value; iteri_ (i+1) f (Cell (c.next))

let iteri = iteri_ 0

let rev_map f l =
    let func acc x = cons (f x) acc in
    fold func Nil l

let rev = fold cons' Nil

let equal eq l1 l2 = match (l1, l2) with
    | (Nil, Nil) -> true
    | (Nil, Cell _)
    | (Cell _, Nil) -> false
    | (Cell c1, Cell c2) ->
        if c1.length != c2.length then
            false
        else
            let rec equal' eq c1 c2 =
                if !(eq c1.value c2.value) then
                    false
                else if c1.length == 0 then
                    true
                else
                    equal' eq c1.next c2.next in
            equal' eq c1 c2

let compare cmp l1 l2 = match (l1, l2) with
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
                if res != 0 || c1.length == 0 then
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

