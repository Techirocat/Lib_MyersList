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
    (k c.length) - 1
  else
    if leaf && (match c.next.more with | Leaf _ -> true | _ -> false) then 
      0 
    else if not leaf then 
      c.jump.rhd - 1
    else 
      c.red - c.next.red
   
(** Forma ineficiente de procurar ponteiro inc, através do RHD desejado *)
let rec find_inc c rhd = match c.more with 
    | Leaf _ ->
        if c.rhd = rhd then
            Some c
        else if c.length = 1 then
            None
        else
            find_inc c.next rhd
    | _ -> find_inc c.next rhd

(** Forma ineficiente de procurar ponteiro uncle, através do RED desejado e K do nó novo *)
let rec find_unc c red kn =
    if c.red = red then
        Some c
    else if k c.length < kn then
        None
    else
        find_unc c.next red kn

(* Essential Functions *)

let cons (v: 'a) (list: 'a t) =
    match list with 
    | Nil -> 
        let c = init v in
        Wrap { head = c; last = c; sigma = 2 }
    | Wrap {head = h; last = l; sigma = s} -> 
        (* 1. definir next e jump, como na improved *)
        let length: int = h.length + 1 in
        let leaf: bool = is_leaf length in
        let next: 'a cell = h in

        let jump: 'a cell = 
            let h_hj = h.length - h.jump.length in
		    if h_hj == 1 || h_hj == l.next.length - l.next.jump.length then 
	  		    l.next
	  	    else 
	  		    l 
        in 

        (* 2. calcular red e rhd *)
        let red: int = calc_red next leaf in 
        let temp: 'a cell =  { next = h; jump = jump; more = Normal; length = length; value = v; rhd = -1; red = red } in 
        let temp_rhd = calc_rhd temp leaf in
        let rhd: int = if temp_rhd < 0 then -1 else temp_rhd in

        (* 3. determinar se é normal, skip ou leaf *)
        let skip: bool = (((k length) - red) mod s = 0) in

        let more = 
            if leaf then 
                (* 4. definir inc *)
                let inc = find_inc h (rhd + 1) in
                (* 5. definir uncle *)
                let unc = find_unc l rhd (k length) in
                Leaf (
                    (match unc with Some i -> i | None -> next), 
                    (match inc with Some i -> i | None -> next)
                )
            else if skip then
                (* 5. definir uncle *)
                let unc = find_unc l rhd (k length) in
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


let lookup l len = failwith "unimplemented"
    (* TODO: Implementar *)

let index l i = lookup l (l.length - i + 1)

(* Library Functions *)

let empty = Nil

(* TODO: adicionar ficheiro .mli e fazer funções *)

(* Debug Functions *) (* substituir por uma suite de testes *)

let _print_info_cell c =
    print_string "k = ";
    print_int (k c.length);
    print_string "  ";

    print_string "len = ";
    print_int c.length;
    print_string "  ";

    print_string "rhd = ";
    print_int c.rhd;
    print_string "  ";

    print_string "red = ";
    print_int c.red;
    print_string "  ";

    print_string "next = ";
    print_int c.next.length;
    print_string "  ";

    print_string "jump = ";
    print_int c.jump.length;
    print_string "  ";
    
    match c.more with
        | Normal -> 
            print_endline "NORMAL  "
        | Skip (u) ->
            print_string "SKIP  ";
            print_string "uncle = ";
            print_int u.length;
            print_string "  ";
            print_newline ();
        | Leaf (u, i) ->
            print_string "LEAF  ";
            print_string "uncle = ";
            print_int u.length;
            print_string "  ";
            print_string "inc = ";
            print_int i.length;
            print_string "  ";
            print_newline ();
    ()

let rec _print_info_list c =
    _print_info_cell c;
    if c.length = 1 then
        ()
    else
        _print_info_list c.next

let _print_info_wrap w = 
    print_endline "head:";
    _print_info_list w.head;
    print_newline ();
    print_endline "last:";
    _print_info_cell w.last;
    ()

let rec print_test n acc =
    if n = 0 then
        match acc with
            | Wrap w -> _print_info_wrap w
            | Nil -> prerr_endline "nil"
    else
        let c = cons n acc in
        print_test (n - 1) c
