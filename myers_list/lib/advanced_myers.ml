(*
type 'a normal_node = { next : 'a cell; jump : 'a cell; length : int; value : 'a; rhd : int; red : int }
and 'a skip_node = { next : 'a cell; jump : 'a cell; uncle : 'a cell; length : int; value : 'a; rhd : int; red : int }
and 'a leaf_node = { next : 'a cell; jump : 'a cell; uncle : 'a cell; inc : 'a cell; length : int; value : 'a; rhd : int; red : int }
and 'a cell = Normal of 'a normal_node | Skip of 'a skip_node | Leaf of 'a leaf_node
*)

(* Additional Pointers: uncle e/ou inc *)
type 'a add_points = Normal | Skip of 'a cell | Leaf of 'a cell * 'a cell
and 'a cell = { next : 'a cell; jump : 'a cell; length : int; value : 'a; rhd : int; red : int; more : 'a add_points }

type 'a wrap = { head : 'a cell; last : 'a cell; sigma : int }

type 'a t = Nil | Wrap of 'a wrap

(* NOTA: LEMBRAR QUE O LENGTH COMEÇA NO 1 *)
(* NOTA: RHD's indefinidos são marcados com números negativos *)

let init (v: 'a) : 'a cell =
    let rec c : 'a cell = { next = c; jump = c; more = Leaf (c, c) ; length = 1; value = v; rhd = -1; red = 0 } in
    c

(* Helper Functions *)

let k len = int_of_float @@ floor @@ Float.log2 @@ float len

let is_not_leaf hnext l =
    let xj = hnext.length - hnext.jump.length in
    let rj = l.next.length - l.next.jump.length in
    xj = 1 || xj = rj

let is_leaf hn l = not (is_not_leaf hn l)

(* Essential Functions *)

let cons (v: 'a) (l: 'a wrap) =
    (* 1. definir next e jump, como na improved *)
    let leaf: bool = is_leaf l.head.next l.last in
    let length: int = l.head.length + 1 in
    let next: 'a cell = l.head in
    let jump: 'a cell =
        if not leaf then
            l.last.next
        else
            l.last in
    (* 2. calcular red e rhd *)
    let red: int =
        (* caso do length = 1 é desnecessario *)
        if leaf then
            k length
        else
            next.red - 1 in
    let rhd: int =
        (* caso do length = 1 é desnecessario *)
        if next.rhd < 0 then
            (k length) - 1
        else if leaf && (match next.more with Leaf _ -> true | _ -> false) then 
            0
        else if not leaf then
            jump.rhd - 1
        else
            red - next.red in
    (* 3. determinar se é normal, skip ou leaf *)
    (* 4. definir inc *)
    (* 5. definir uncle *)
    l (* remover dps *)

let lookup l len = failwith "unimplemented"
    (* TODO: Implementar *)

let index l i = lookup l (l.length - i + 1)

(* Library Functions *)

let empty = Nil

(* TODO: adicionar ficheiro .mli e fazer funções *)


let is_leaf (len : int) : bool = (*Recebe como argumento o length da nova célula a ser inserida*)
    let rec aux len = 
        match len with 
        | 1 -> true 
        | 0 -> false 
        | _ -> 
            let rec largest_tree k = 
			    if (1 lsl (k+1)) - 1 <= len then 
	  			    largest_tree (k+1) 
                else 
                    k
            in
		    let k = largest_tree 0 in
            let resto = len - ((1 lsl k)- 1) in 
            aux resto 
    in aux len 


