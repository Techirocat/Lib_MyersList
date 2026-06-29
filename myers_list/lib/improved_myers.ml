type 'a cell = { 
  next : 'a cell; 
  jump : 'a cell; 
  length : int; 
  value : 'a 
}

type 'a wrap = { 
  head : 'a cell; 
  last : 'a cell 
}

type 'a t = 
  | Empty
  | Wrap of {head : 'a cell; last : 'a cell}


let empty : 'a t = Empty

let is_empty (wrap : 'a t) : bool =
  match wrap with
  | Empty -> true
  | Wrap _ -> false

let init' (v : 'a) : 'a cell =
  let rec c : 'a cell = { next = c; jump = c; length = 0; value = v } in c
[@@inline]

let init (v : 'a) : 'a t =
  let c : 'a cell = init' v in 
  Wrap { head = c; last = c }
[@@inline]


let cons (v : 'a) (wrap : 'a t) : 'a t =
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

let hd (wrap : 'a t) : 'a = 
  match wrap with 
  | Empty -> failwith ".hd: Argumento inválido"
  | Wrap {head = h; last = _} -> h.value

let return (v : 'a) : 'a t = init v

(**
let rec lookup (c : 'a cell) (l : int) : 'a cell =
  if c.length = l
  then c
  else let j = c.jump in
       let c' = if j.length < l then c.next else j in
       lookup c' l

let index (c : 'a cell) (i : int) : 'a cell = lookup c (c.length - i)
[@@inline]

(* Avoiding wrap until the top level improves the performance:
| make-list/3lgn/1_000_000 |     0.99 |  67.99ms | -2.29% +2.53% |  5.00Mw |   4.87Mw |   4.87Mw |     98.12% |
| make-list/2lgn/1_000_000 |     0.99 |  69.29ms | -2.16% +1.56% |  5.00Mw |   4.88Mw |   4.88Mw |    100.00% |
*)

let make_list_rev' (v : 'a) (vs : 'a list) : 'a wrap =
  let rec go (h : 'a cell) (l : 'a cell) : 'a list -> 'a wrap = function
  | [] -> { head = h; last = l }
  | v :: vs ->
    let h_len = h.length in
    let h_hj = h_len - h.jump.length in
    if h_hj == 1 || h_hj == l.next.length - l.next.jump.length
    then let c = { next = h; jump = l.next; length = h_len + 1; value = v } in
         go c l.jump vs
    else let c = { next = h; jump = l; length = h_len + 1; value = v } in
         go c c vs in
  let last = init' v in
  go last last vs

(* Equivalent code but adds a wrap at each step

| make-list/3lgn/1_000_000 |     1.00 |  69.88ms | -1.70% +2.11% |  5.00Mw |   4.87Mw |   4.87Mw |     93.17% |
| make-list/2lgn/1_000_000 |     0.99 |  75.00ms | -1.60% +2.86% |  8.00Mw |   4.91Mw |   4.91Mw |    100.00% |

*)
let make_list_rev (v : 'a) (vs : 'a list) : 'a wrap =
  let rec go (b : 'a wrap) : 'a list -> 'a wrap = function
  | [] -> b
  | v :: vs -> go (cons v b) vs in
  go (init v) vs

let make_tree_rev (height : int) (v : 'a) (vs : 'a list) : 'a wrap =
  let rec go (last : 'a cell) (last_len : int) (vs : 'a list) : int -> 'a cell * 'a list = function
  | 0 -> (last, vs)
  | h -> let right, v' :: vs' = go last last_len vs (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let left_last_len = last_len + 1 lsl h - 1 in
         let left_last = { next = right; jump = last; length = left_last_len; value = v' } in
         let left, v'' :: vs'' = go left_last left_last_len vs' (h - 1)
             [@@warning "-8"] (* Warning 8 = this pattern-matching is not exhaustive. *) in
         let c = { next = left; jump = right; length = last_len + 2 lsl h - 2; value = v'' } in
         c, vs'' in
  let last = init' v in
  let c, _ = go last 0 vs (height - 1) in
  { head = c; last = last }

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

*)