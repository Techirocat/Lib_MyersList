(* .mli Improved Myers List*)

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

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Map on elements*)


val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** Map with index*)

val hd : 'a t -> 'a
(**
    First element of the list, or 
    Raise Invalid_argument
    if the list is empty
*)

val last : 'a t -> 'a
(** last l returns the last element of l. O(log(n))
    Raise Invalid_argument 
    if the list is empty
*)

val tl : 'a t -> 'a t
(** Remove the first element from the list, or
    Raises Invalid_argument
    if the list is empty
*)

val front : 'a t -> ('a * 'a t) option
(** Remove and return the firs element of the list*)

val front_exn : 'a t -> 'a * 'a t
(** Unsafe version of front 
    Raises Invalid_argument
    if the list is empty
*)

val length : 'a t -> int 
(** Number of elements*)

val get : 'a t -> int -> 'a option
(** get l i accesses the i-th element of the list. O(log(n))*)

val get_exn : 'a t -> int -> 'a 
(** Unsafe version of get
    Raises Invalid_argument
    if the list has less than i+1 elements
*)

val set : 'a t -> int -> 'a -> 'a t 
(** set l i v sets the i-th element of the list to v. O(i)
    Raise Invalid_argument
    if the list has les than i+1 elements or is empty
*)

val remove : 'a t -> int -> 'a t
(** remove l i returns a copy of l without its i-th element. O(i)
    Raise Invalid_argument 
    if the list has less than i+1 elements or is empty
*)

val get_and_remove_exn : 'a t -> int -> 'a * 'a t
(** get_and_remove_exn l i accesses and removes the i-th element of l
    Raise Invalid_argument
    if the list has less than i+1 elements
*)

val append : 'a t -> 'a t -> 'a t
(** append l1 l2 returns a new list containing the elements of l1
    followed by the elements of l2.
*)

val filter : f:('a -> bool) -> 'a t -> 'a t
(** filter f l returns all the elements of the list l that satisfy the predicate f*)

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
(** filter_map f l applies f to every element of l and keeps only
    the elements for which f returns Some _, unwrapping the option. *)


val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(** flat_map f l applies f to every element of l, producing a list
    for each one, and concatenates all the resulting lists in order. *)


val flatten : 'a t t -> 'a t
(** flatten l concatenates a list of lists into a single list,
    preserving the order of both the outer and the inner lists. *)

val app : ('a -> 'b) t -> 'a t -> 'b t
(* app funs l applies every function to every value (Cartesian product). Ex: [f; g] applied to [1; 2] results in [f 1; f 2; g 1; g 2]. *)

val take : int -> 'a t -> 'a t
(** take n l returns the prefix of l of length n, or l if n > length*)

val take_while : f:('a -> bool) -> 'a t -> 'a t
(** take_while p l is the longest (possibly empty) prefix of l containing only elements that satisfy p.*)

val drop : int -> 'a t -> 'a t
(**drop n l returns the suffix of l after skipping n elements*)

val drop_while : f:('a -> bool) -> 'a t -> 'a t
(** drop_while p l drops elements from the front of l as long as p evaluates to true, returning the remaining list. *)

val take_drop : int -> 'a t -> 'a t * 'a t
(* take_drop n l splits l into a, b such that length a = n if length l >= n, and such that append a b = l. *)

val iter : ('a -> unit) -> 'a t -> unit
(** iterate on the list's elements*)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(** Same as iter, but the function is applied to the index of the element as first argument, 
and the element itself as second argument*)

val fold : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b
(** fold on the list's elements*)

val fold_rev : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b
(* A implementação que eu fiz não é tail recursive, pode ser  preciso alterar*)

val rev_map : f:('a -> 'b) -> 'a t -> 'b t
(** rev_map f l is the same as map f (rev l)*)

val rev : 'a t -> 'a t
(** Reverse the list*)

val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(** equal eq l1 l2 returns true if l1 and l2 have the same length
    and eq returns true on every pair of elements at the same
    position, false otherwise. *)


val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** compare cmp l1 l2 performs a lexicographic comparison of l1 and
    l2 using cmp on their elements. If one list is a strict prefix
    of the other, the shorter list is considered smaller. *)


(** UTILS*)

val make : int -> 'a -> 'a t
(** make n v returns a list of length n where every element is v. *)

val repeat : int -> 'a t -> 'a t
(** repeat n l is append l (append l ... l) n times.*) 

val range : int -> int -> int t
(** range i j is i; i+1; ... ; j or j; j-1; ...; i.*)


(** CONVERSIONS*)

type 'a iter = ('a -> unit) -> unit

type 'a gen = unit -> 'a option


(** LIST*)

val add_list : 'a t -> 'a list -> 'a t
(** add_list l elts returns a new list containing the elements of
    elts (in the same order) followed by the elements of l. *)

val of_list : 'a list -> 'a t
(** Converts a list to a Improved Myers List*)

val to_list : 'a t -> 'a list 
(** to_list l return the list of all the elements of l*)

val of_list_map : f:('a -> 'b) -> 'a list -> 'b t
(** Combination of of_list and map*)


(** ARRAY*)

val add_array : 'a t -> 'a array -> 'a t
(** add_array l arr returns a new list containing the elements of
    arr (in the same order) followed by the elements of l. *)

val of_array : 'a array -> 'a t
(** Converts an array into a Improved Myers List*)

val to_array : 'a t -> 'a array
(** to_array l returns an array containing all the elements of l*)


(** ITERATOR*)

val add_iter : 'a t -> 'a iter -> 'a t
(** add_iter l it returns a new list containing the elements produced
    by it (in the same order) followed by the elements of l. *)

val of_iter : 'a iter -> 'a t
(** Converts a iterator into a Improved Myers List*)

val to_iter : 'a t -> 'a iter
(** to_iter l returns a iterator of the list l*)


(** GENERATOR*)

val add_gen : 'a t -> 'a gen -> 'a t
(** add_gen l g returns a new list containing the elements produced
    by g (in the same order) followed by the elements of l. *)


val of_gen : 'a gen -> 'a t
(** of_gen g consumes the generator g entirely and returns a list
    containing the elements it produced, in the order they were
    produced. *)

val to_gen : 'a t -> 'a gen
(** to_gen l returns a generator that yields the elements of l, in
    order, one at a time, and None once every element has been
    produced. *)


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


(** IO *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : ?pp_sep:unit printer -> 'a printer -> 'a t printer

