(* Original: 3 floor(lg(n+1)) - 2 *)

type 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a }

(**************************************)
(* High-performance functions *)

let init (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; jump = c; length = 0; value = v } in
  c
[@@inline]

let cons (v : 'a) (c : 'a cell) : 'a cell =
  let j = c.jump in
  let j' = if c.length - j.length == j.length - j.jump.length then j.jump else c in
  { next = c; jump = j'; length = c.length + 1; value = v }
[@@inline]

let rec lookup (c : 'a cell) (l : int) : 'a cell =
  if c.length = l
  then c
  else let j = c.jump in
       let c' = if j.length < l then c.next else j in
       lookup c' l

let index (c : 'a cell) (i : int) : 'a cell = lookup c (c.length - i)
[@@inline]

let make_list_rev (v : 'a) (vs : 'a list) : 'a cell =
  let rec go (c : 'a cell) : 'a list -> 'a cell = function
  | [] -> c
  | v :: vs -> go (cons v c) vs in
  go (init v) vs

let make_tree_rev (height : int) (v : 'a) (vs : 'a list) : 'a cell =
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
  let c, _, _ = go (init v) 0 vs (height - 1) in
  c

(**************************************)
(* Non-high-performance functions *)

let find_path (c : 'a cell) (l : int) : 'a cell list =
  let rec go (c : 'a cell) (l : int) (p : 'a cell list) : 'a cell list =
    if c.length = l
    then p
    else let j = c.jump in
         let c' = if j.length < l then c.next else j in
         go c' l (c' :: p) in
  go c l [c]

let index_path (c : 'a cell) (i : int) : 'a cell list = find_path c (c.length - i)
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
