(* .mli Improved Myers List*)

(*
TODO:
- Criar testes e testar todas as Funções
- ocamldoc
- Colocar execeptions melhores
*)


(**
Random-Access Lists 

This is an OCaml implementation of the "Improved Myers List" data structure, as described in the paper  
"Pushing the Information-Theoretic Limits of Random Access Lists" by Edwars Peter, Yong Qi Foo and Michael D. Adams. 
It defines a list-like data structure (that can be visualized as a tree) with O(1) cons/tail operations, and O(log(n)) lookup.
*)


type 'a t
(** List containing elements of type 'a*)

val empty : 'a t
(** Empty list*)

val is_empty : 'a t -> bool
(** Check whether the list is empty*)

val cons : 'a -> 'a t -> 'a t
(** Add an element at the front of the list*)

val return : 'a -> 'a t
(** Singleton *)

val map : ('a -> 'b) -> 'a t -> 'b t
(** Map on elements*)


val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t
(** Map with index*)

val hd : 'a t -> 'a
(**
    First element of the list, or 
    Raise ...
    if the list is empty
*)

val last : 'a t -> 'a
(** AINDA VAZIO*)

val tl : 'a t -> 'a t
(** Remove the first element from the list, or
    Raises ...
    if the list is empty
*)

val front : 'a t -> ('a * 'a t) option
(** Remove and return the firs element of the list*)

val front_exn : 'a t -> 'a * 'a t
(** Unsafe version of front 
    Raises ...
    if the list is empty
*)

val length : 'a t -> int 
(** Number of elements*)

val get : 'a t -> int -> 'a option
(** get l i accesses the i-th element of the list. O(log(n))*)

val get_exn : 'a t -> int -> 'a 
(** Unsafe version of get
    Raises ...
    if the list has less than i+1 elements
*)

val set : 'a t -> int -> 'a -> 'a t 
(** set l i v sets the i-th element of the list to v. O(i)
    Raise ...
    if the list has les than i+1 elements or is empty
*)

val remove : 'a t -> int -> 'a t
(** AINDA VAZIO*)

val get_and_remove_exn : 'a t -> int -> 'a * 'a t
(** get_and_remove_exn l i accesses and removes the i-th element of l
    Raise ...
    if the list has less than i+1 elements
*)

val append : 'a t -> 'a t -> 'a t
(** Ainda vazio*)

val filter : ('a -> bool) -> 'a t -> 'a t
(** filter f l returns all the elements of the list l that satisfy the predicate f*)

val filter_map : ('a -> 'b option) -> 'a t -> 'b t
(** AINDA VAZIO*)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** AINDA VAZIO*)

val flatten : 'a t t -> 'a t
(** AINDA VAZIO*)

val app : ('a -> 'b) t -> 'a t -> 'b t
(* app funs l applies every function to every value (Cartesian product). Ex: [f; g] applied to [1; 2] results in [f 1; f 2; g 1; g 2]. *)

val take : int -> 'a t -> 'a t
(** take n l returns the prefix of l of length n, or l if n > length*)

val take_while : ('a -> bool) -> 'a t -> 'a t
(** take_while p l is the longest (possibly empty) prefix of l containing only elements that satisfy p.*)

val drop : int -> 'a t -> 'a t (** TODO: Refazer com a ideia da função set*)
(** AINDA VAZIO - Rever esta função e tentar implementaar de forma a partilhar os elementos com as outras lista e não criar uma lista nova do zero*)

val drop_while : ('a -> bool) -> 'a t -> 'a t (** TODO: Refazer com a ideia da função set*)
(** Mesma coisa que em drop*)

val take_drop : int -> 'a t -> 'a t * 'a t
(* take_drop n l splits l into a, b such that length a = n if length l >= n, and such that append a b = l. *)

val iter : ('a -> unit) -> 'a t -> unit
(** iterate on the list's elements*)

val iteri : (int -> 'a -> unit) -> 'a t -> unit
(** Same as iter, but the function is applied to the index of the element as first argument, 
and the element itself as second argument*)

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(** fold on the list's elements*)

val fold_rev : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(* A implementação que eu fiz não é tail recursive, pode ser  preciso alterar*)

val rev_map : ('a -> 'b) -> 'a t -> 'b t
(** rev_map f l is the same as map f (rev l)*)

val rev : 'a t -> 'a t
(** Reverse the list*)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** AINDA VAZIO*)

val compare : ('a -> 'a -> int) -> 'a t -> 'a t -> int
(** AINFA VAZIO*)


(** UTILS*)

val make : int -> 'a -> 'a t
(** AINDA VAZIO*)

val repeat : int -> 'a t -> 'a t
(** repeat n l is append l (append l ... l) n times.*) 

val range : int -> int -> int t
(** range i j is i; i+1; ... ; j or j; j-1; ...; i.*)


(** CONVERSIONS*)

type 'a iter = ('a -> unit) -> unit
(** AINDA VAZIO*)

type 'a gen = unit -> 'a option
(** AINDA VAZIO*)


(** LIST*)

val add_list : 'a t -> 'a list -> 'a t
(**AINDA VAZIO*)

val of_list : 'a list -> 'a t
(** Converts a list to a Improved Myers List*)

val to_list : 'a t -> 'a list 
(** to_list l return the list of all the elements of l*)

val of_list_map : ('a -> 'b) -> 'a list -> 'b t
(** Combination of of_list and map*)


(** ARRAY*)

val add_array : 'a t -> 'a array -> 'a t
(** AINDA VAZIO*)

val of_array : 'a array -> 'a t
(** Converts an array into a Improved Myers List*)

val to_array : 'a t -> 'a array
(** to_array l returns an array containing all the elements of l*)


(** ITERATOR*)

val add_iter : 'a t -> 'a iter -> 'a t
(** AINDA VAZIO*)

val of_iter : 'a iter -> 'a t
(** Converts a iterator into a Improved Myers List*)

val to_iter : 'a t -> 'a iter
(** to_iter l returns a iterator of the list l*)


(** GENERATOR*)

val add_gen : 'a t -> 'a gen -> 'a t
(** AINDA VAZIO*)

val of_gen : 'a gen -> 'a t
(** AINDA VAZIO*)

val to_gen : 'a t -> 'a gen
(** AINDA VAZIO*)


(** INFIX*)

module Infix : sig

    val (@+) : 'a -> 'a t -> 'a t
    (** Cons (alias to cons).*)

    val (>>=) : 'a t -> ('a -> 'b t) -> 'b t
    (**Alias to flat_map.*)

    val (>|=) : 'a t -> ('a -> 'b) -> 'b t
    (**Alias to map*)

    val (<*>) : ('a -> 'b) t -> 'a t -> 'b t
    (** Alias to app*)

    val (--) : int -> int -> int t
    (**Alias to range*)

    val (--^) : int -> int -> int t
    (**a --^ b is the integer range from a to b, where b is excluded.*)

end 

include module type of Infix

