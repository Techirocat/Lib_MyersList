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

(* 

let is_not_leaf hnext l =
    let xj = hnext.length - hnext.jump.length in
    let rj = l.next.length - l.next.jump.length in
    xj = 1 || xj = rj

let is_leaf hn l = not (is_not_leaf hn l)

*)


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



let calc_height (c : 'a cell) : int =
  let rec largest_tree k = 
    if (1 lsl (k + 1)) - 1 <= c.length then 
      largest_tree (k + 1) 
    else 
      k
  in
  largest_tree 0




(*Eu fiz este calculo em uma função a parte, mas se preferires coloca dentro do cons*)
let calc_red (next : 'a cell) (leaf : bool) : int =  
  let len = next.length + 1 in
  match len with 
  | 1 -> 0
  | _ -> 
    match leaf with 
    | true -> k len 
    | false -> next.red - 1 



let calc_rhd (c : 'a cell) (leaf : bool) : int = 
  if c.next.rhd < 0 then 
    (k c.length) + 1
  else
    if leaf && (match c.next.more with | Leaf _ -> true | _ -> false) then 
      0 
    else if not leaf then 
      c.jump.rhd - 1
    else 
      c.red - c.next.red
   

(* Essential Functions *)

let cons (v: 'a) (list: 'a wrap) =
    match list with 
    | Nil -> init v 
    | Wrap {head = h; last = l; sigma = s} -> 
      (* 1. definir next e jump, como na improved *)
      let length: int = h.length + 1 in
      let leaf: bool = is_leaf length in
      let next: 'a cell = h in

      (** Eu defeni o jump exatamente como esta na Improved, mas tu tinhas diferente inicialmente, 
      vê lá qual é a maneira certa que estavas a pensar, e deculpa-me se eu alterei de forma errada*)
      let jump: 'a cell = 
        let h_hj = h_len - h.jump.length in
		    if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then 
	  		  l.next
	  	  else 
	  		  l 
      in 

      (* 2. calcular red e rhd *)
      let red : int = calc_red next leaf in 

      let temp : 'a cell =  { next = h; jump = jump; more = Normal; length = length; value = v; rhd = -1; red = red } in 

      let rhd : int = calc_rhd temp leaf in


    (* 3. determinar se é normal, skip ou leaf *)

      if leaf then 
        (* 4. definir inc *)
        (* 5. definir uncle *)

      else
       (* Decubrir se ele é Normal ou Skip - precisamos saber a altura para saber o que fazer aqui*)  
       
    (* remover dps *)

let lookup l len = failwith "unimplemented"
    (* TODO: Implementar *)

let index l i = lookup l (l.length - i + 1)

(* Library Functions *)

let empty = Nil

(* TODO: adicionar ficheiro .mli e fazer funções *)



