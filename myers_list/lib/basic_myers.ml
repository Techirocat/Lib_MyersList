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
