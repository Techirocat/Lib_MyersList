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

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option


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

let length (wrap : 'a t) : int = 
  match wrap with 
  | Empty -> 0 
  | Wrap {head = h; last = _} -> h.length + 1

let rec lookup (c : 'a cell) (l : int) : 'a cell =
  if c.length = l
  then c
  else let j = c.jump in
       let c' = if j.length < l then c.next else j in
       lookup c' l


let get (wrap : 'a t) (i : int) : 'a option = 
  match wrap with 
  | Empty -> None
  | Wrap {head = h; last = _} -> 
    if i < 0 || i > h.length then 
      None
    else 
      Some (lookup h (h.length - i)).value


let get_exn (wrap : 'a t) (i : int) : 'a = 
  match wrap with 
  | Empty -> failwith ".get_exn : Lista vazia"
  | Wrap {head = h; last = _} -> 
    if i < 0 || i > h.length then 
      failwith ".get_exn : Index invalido" 
    else 
      (lookup h (h.length - i)).value
(*

let set (wrap : 'a t) (i : int) (v : 'a) : 'a t =
  match wrap with
  | Empty -> failwith ".set: Lista vazia"
  | Wrap {head = h; last = l} ->
    if i < 0 || i >= h.length + 1 then failwith ".set: Index invalido"
    else
      let len = h.length - i in
      let new_last = ref l in
      let rec path (c : 'a cell) : 'a cell =

        if c.length = len then begin
          let c' = { value = v; length = c.length; next = c.next; jump = c.jump } in
          if c.length = l.length then new_last := c';
          c'
        end 

        else begin
          let p = path c.next in
          let c' = { value = c.value; length = c.length; next = p; jump = c.jump } in
          if c.length = l.length then new_last := c';
          c'
        end
      in let new_head = path h in Wrap { head = new_head; last = !new_last } 
*)      

let add_list (wrap : 'a t) (l : 'a list) : 'a t = List.fold_left (fun acc v -> cons v acc) wrap l  



let of_list (l : 'a list) : 'a t = add_list empty l

let to_list_rev (wrap : 'a t) : 'a list = 
  match wrap with 
  | Empty -> [] 
  | Wrap {head = h; last = _} -> 
    let rec aux acc c = 
      if c.length = 0 then 
        (c.value :: acc)
      else aux (c.value :: acc) c.next
    in aux [] h
   
let to_list (wrap : 'a t) : 'a list = List.rev (to_list_rev wrap) 

let to_list_map_rev (wrap : 'a t) (f : 'a -> 'b) : 'b list = 
  match wrap with 
  | Empty -> [] 
  | Wrap {head = h; last = _} -> 
    let rec aux acc c = 
      if c.length = 0 then 
        ((f c.value) :: acc)
      else aux ((f c.value) :: acc) c.next
    in aux [] h
 
let to_list_map (wrap : 'a t) (f : 'a -> 'b) : 'b list = List.rev (to_list_map_rev wrap f)

let to_list_mapi_rev (wrap : 'a t) (f : int -> 'a -> 'b) : 'b list = 
  match wrap with 
  | Empty -> [] 
  | Wrap {head = h; last = _} -> 
    let rec aux acc c = 
      if c.length = 0 then 
        ( (f c.length c.value) :: acc)
      else aux ((f c.length c.value) :: acc) c.next
    in aux [] h 

let to_list_filter_rev (wrap : 'a t) (f : 'a -> bool) : 'a list =
   match wrap with 
   | Empty -> []
   | Wrap {head = h; last = _} -> 
    let rec aux acc c = 
      if c.length = 0 then
        if (f c.value) then (c.value :: acc) 
        else acc 
      else 
        if (f c.value) then aux (c.value :: acc) c.next 
        else aux acc c.next
    in aux [] h 


let to_list_filter (wrap : 'a t) (f : 'a -> bool) : 'a list = List.rev (to_list_filter_rev wrap f)

let filter (f : 'a -> bool) (wrap : 'a t) : 'a t = 
  let l = to_list_filter_rev wrap f in of_list l  


let add_list_map (wrap : 'b t) (l : 'a list) (f : 'a -> 'b) : 'b t = List.fold_left (fun acc v -> cons (f v) acc) wrap l


let map (f : 'a -> 'b)  (wrap : 'a t) : 'b t = 
  match wrap with 
  | Empty -> Empty    
  | Wrap _-> let l = to_list_map_rev wrap f in of_list l 


let mapi (f : int -> 'a -> 'b) (wrap : 'a t) : 'b t = 
  match wrap with 
  | Empty -> Empty 
  | Wrap _ -> let l = to_list_mapi_rev wrap f in of_list l


let of_list_map (f : 'a -> 'b) (l : 'a list) : 'b t = add_list_map empty l f


let make (n : int) (v : 'a) : 'a t = 
  let rec aux n acc v = 
    if n <= 0 then acc else aux (n-1) (cons v acc) v
  in aux n empty v


let append (l0 : 'a t) (l1 : 'a t) : 'a t =
  let a = to_list_rev l0 in add_list l1 a


let repeat (n : int) (l : 'a t) : 'a t =
  let a = to_list_rev l in  
  let rec aux acc n = 
    if n <= 0 then acc 
    else aux (add_list acc a) (n-1)
  in aux empty n 


let range i j = 
  let rec aux i j acc = 
    if i = j then cons i acc 
    else if i < j then aux i (j-1) (cons j acc) 
    else aux i (j+1) (cons j acc)
  in aux i j empty 


let equal (eq : 'a -> 'a -> bool) (w1 : 'a t) (w2 : 'a t) : bool = 
  match w1, w2 with 
  | Empty, Empty -> true
  | Empty, Wrap _ -> false
  | Wrap _, Empty -> false
  | Wrap {head = h1; last = _}, Wrap {head = h2; last = _} ->
    if h1.length != h2.length then false 
    else 
      let rec aux c1 c2 = 
        if not (eq c1.value c2.value) then false 
        else aux c1.next c2.next
      in aux h1 h2 


let iter (f : 'a -> unit) (wrap : 'a t) : unit = 
  match wrap with 
  | Empty -> () 
  | Wrap {head = h; last = _ } -> 
    let rec aux c =
      if c.length = 0 then f c.value 
      else  f c.value; aux c.next
    in aux h



let iteri (f : int -> 'a -> unit) (wrap : 'a t) : unit = 
  match wrap with 
  | Empty -> () 
  | Wrap {head = h; last = _} -> 
    let rec aux c = 
      if c.length = 0 then (f c.length c.value)
      else (f c.length c.value); aux c.next 
    in aux h 


let fold (f : 'b -> 'a -> 'b)  (x : 'b) (wrap : 'a t) : 'b = 
    match wrap with 
    | Empty -> x 
    | Wrap {head = h; last = _} -> 
      let rec aux acc c = 
        if c.length = 0 then f x c.value 
        else aux (f acc c.value) c.next 
      in aux x h 



let fold_rev (f : 'b -> 'a -> 'b)  (x : 'b) (wrap : 'a t) : 'b = 
    match wrap with 
    | Empty -> x 
    | Wrap {head = h; last = _} ->
      let rec aux c = 
        if c.length = 0 then f x c.value 
        else f (aux c.next) c.value 
      in aux h 


let rev  (wrap : 'a t) : 'a t = 
    match wrap with 
    | Empty -> Empty
    | Wrap _-> 
      let list = to_list_rev wrap in of_list list
  

let rev_map (f : 'a -> 'b) (wrap : 'a t) : 'b t = 
  match wrap with 
  | Empty -> Empty
  | Wrap _ -> 
    let list = to_list_map_rev wrap f in of_list list 
     

let add_array (wrap :'a t) (arr : 'a array) : 'a t = Array.fold_left (fun acc v -> cons v acc) wrap arr
 
let of_array (arr : 'a array) : 'a t = add_array empty arr


let to_array (wrap : 'a t) : 'a array = 
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
 

let filter_map (f : 'a -> 'b option) (wrap : 'a t) : 'b t = 
   match wrap with
   | Empty -> Empty
   | Wrap _ ->

    let func acc v = 
      match f v with 
      | Some x -> x :: acc
      | None -> acc
    in  
    let list = fold func [] wrap in of_list list


let flat_map (f : 'a -> 'b t) (wrap : 'a t) : 'b t =
  fold_rev (fun acc v -> append acc (f v)) empty wrap


let take (n : int) (wrap : 'a t) : 'a t = 
  match wrap with 
  | Empty -> Empty 
  | Wrap {head = h; last = _} -> 
    let rec aux acc c i =
      if i >= n then
        of_list acc
      else if c.length = 0 then 
        of_list (c.value :: acc) 
      else aux (c.value :: acc) c.next (i + 1)  
    in aux [] h 0

let take_while  (f : 'a -> bool)  (wrap : 'a t) : 'a t = 
  match wrap with 
  | Empty -> Empty
  | Wrap {head = h; last = _} -> 
      let rec aux acc c = 
        if (f c.value) then 
          if c.length = 0 then of_list (c.value :: acc)
          else aux (c.value :: acc) c.next
        else of_list acc 
      in aux [] h


let drop (n : int) (wrap : 'a t) : 'a t = 
  match wrap with 
  | Empty -> Empty
  | Wrap {head = h; last = _} -> 
    if n < 0 || n > h.length then failwith "Argumento inválido" 
    else 
      let rec aux acc c i = 
        if i <= n then aux acc c.next (i+1) 
        else 
          if c.length = 0 then 
            of_list (c.value :: acc)
          else
            aux (c.value :: acc) c.next (i+1)
      in aux [] h 0

let drop_while (f : 'a -> bool) (wrap : 'a t) : 'a t =
  match wrap with
  | Empty -> Empty
  | Wrap {head = h; last = _} ->
    let rec count n c =
      if f c.value then
        if c.length = 0 then n + 1
        else count (n + 1) c.next
      else n
    in
    drop (count 0 h) wrap


(*
    n=3   [1; 2; 3; 4; 5; 6; 7; 8]

    Resultado:
    acc1 = [1; 2; 3]
    acc2 = [4; 5; 6; 7; 8]


    n=8   [1; 2; 3; 4; 5; 6; 7; 8]

    Resultado: 
    acc1 = [1; 2; 3; 4; 5; 6; 7; 8]
    acc2 = Empty
*)

let take_drop (n : int) (wrap : 'a t) : 'a t * 'a t = take n wrap, drop n wrap 



let add_iter (wrap : 'a t) (s : 'a iter) : 'a t = 
  let l1 = ref empty in s (fun x -> l1 := cons x !l1); 
  fold (fun acc x -> cons x acc) wrap !l1

let of_iter  (s : 'a iter) : 'a t =  add_iter empty s 

let to_iter (wrap : 'a t) : 'a iter =  fun f -> iter f wrap

let rec gen_iter_ f g = match g() with 
  | None -> ()
  | Some x -> f x; gen_iter_ f g

let add_gen (wrap : 'a t) (g : 'a gen) : 'a t = 
  let w1 = ref empty in 
  gen_iter_  (fun x -> w1 := cons x !w1) g; 
  fold (fun acc x -> cons x acc) wrap !w1

let of_gen  (g : 'a gen) : 'a t = add_gen empty g 

let to_gen (wrap : 'a t) : 'a gen = 
  match wrap with 
  | Empty -> (fun () -> None)
  | Wrap {head = h; last = _} -> 
    let curr = ref h in 
    let flag = ref false in 
    let next () = 
      if !flag then None 
      else begin 
        let va = !curr.value in 
        if !curr.length = 0 then 
          flag := true
        else curr := !curr.next;
        Some va 
      end
    in next















(*

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
