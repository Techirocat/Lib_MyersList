(** .mli Improved Myers List*)

(*
    Original file in: https://github.com/c-cube/ocaml-containers/blob/main/src/data/CCRAL.mli
    Copyright (c) 2013, Simon Cruanes
    All rights reserved.
*)

(**
Random-Access Lists 

This is an OCaml implementation of the "Improved Myers List" data structure, as described in the paper  
"Pushing the Information-Theoretic Limits of Random Access Lists" by Edwars Peter, Yong Qi Foo and Michael D. Adams. 
It defines a list-like data structure (that can be visualized as a tree) with O(1) cons/tail operations, and O(log(n)) lookup.
*)

type +'a t
(** List containing elements of type ['a] *)

val empty : 'a t
(** Empty list. *)

val is_empty : _ t -> bool
(** [is_empty l] is true if and only if [l] has no elements. It is equivalent to [length l = 0]*)

val cons : 'a -> 'a t -> 'a t
(** Add an element at the front of the list.
    [cons x xs] is [x @+ xs]*)

val return : 'a -> 'a t
(** [return x] returns the one-element list [[x]]*)

val map : f:('a -> 'b) -> 'a t -> 'b t
(** Map on elements. 
    [map f [a1; ...; an]] applies function [f] to [a1, ..., an], 
    and builds the list [f a1; ...; f an] with the results returned by [f]
*)

val mapi : f:(int -> 'a -> 'b) -> 'a t -> 'b t
(** Map with index. 
    Same as {!map}, but the function is applied to the index of the element as first argument 
    (counting from 0), and the element itself as second argument.
*)

val hd : 'a t -> 'a
(** Returns first element of the given list
    @raise Invalid_argument if the list is empty. 
*)

val tl : 'a t -> 'a t
(** Return the given list without its first element.
    @raise Invalid_argument if the list is empty. 
*)

val last : 'a t -> 'a
(** Last element of the list.
    @raise Invalid_argument if the list is empty. 
*)

val front : 'a t -> ('a * 'a t) option
(**Returns [Some (head, tail)] where [head] is the first element and [tail] is the rest of the list, or [None] if the list is empty.*)

val front_exn : 'a t -> 'a * 'a t
(** Unsafe version of {!front}. Returns [(head, tail)].
    @raise Invalid_argument if the list is empty. 
*)

val length : 'a t -> int
(** Return the length (number of elements) of the given list *)

val get : 'a t -> int -> 'a option
(** [get l i] accesses the [i]-th element of the list. [O(log(n))]. The first element (head of the list) is at position 0*)

val get_exn : 'a t -> int -> 'a
(** Unsafe version of {!get}.
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val set : 'a t -> int -> 'a -> 'a t
(** [set l i v] sets the [i]-th element of the list to [v]. [O(log(n))].
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val remove : 'a t -> int -> 'a t
(** [remove l i] removes the [i]-th element of [l].
    @raise Invalid_argument if the list has less than [i+1] elements. *)

val get_and_remove_exn : 'a t -> int -> 'a * 'a t
(** [get_and_remove_exn l i] accesses and removes the [i]-th element of [l].
    @raise Invalid_argument if the list has less than [i+1] elements. 
*)

val append : 'a t -> 'a t -> 'a t
(**[append l0 l1] appends [l1] to [l0].
*)

val filter : f:('a -> bool) -> 'a t -> 'a t
(**[filter f l] returns all the elements of the list [l] that satisfy the predicate [f].
    The order of the elements in the input list is preserved 
*)

val filter_map : f:('a -> 'b option) -> 'a t -> 'b t
(**[filter_map f l] applies [f] to every element of [l], filters out the None elements and 
    returns the list of the arguments of the Some elements
*)

val flat_map : ('a -> 'b t) -> 'a t -> 'b t
(**[flat_map f l] maps [f] over [l] and flattens the resulting lists into a single list.
*)

val flatten : 'a t t -> 'a t
(**[flatten l] concatenates a list of lists into a single list.
*)

val app : ('a -> 'b) t -> 'a t -> 'b t
(** [app fs xs] applies a list of functions [fs] to a list of elements [xs], returning a list of all resulting combinations.
*)

val take : int -> 'a t -> 'a t
(**[take n l] returns the prefix of [l] of length n, or a copy of [l] if [n > length l]. This is the empty list if [n] is negative
*)

val take_while : f:('a -> bool) -> 'a t -> 'a t
(**[take_while p l] is the longest (possibily empty) prefix of [l] containing only elements that satisfy p.
*)

val drop : int -> 'a t -> 'a t
(**[drop n l] returns the suffix of [l] after [n] elements. 
    @raise Invalid_argument if [n] is negative or greater than [length l + 1].
*)

val drop_while : f:('a -> bool) -> 'a t -> 'a t
(**[drop_while p l] is the longest (possibly empty) suffix of [l] starting at the first element that does not satisfy [p]*)

val take_drop : int -> 'a t -> 'a t * 'a t
(** [take_drop n l] splits [l] into [a, b] such that [length a = n]
    if [length l >= n], and such that [append a b = l]. 
*)

val iter : f:('a -> unit) -> 'a t -> unit
(** Iterate on the list's elements.
    [iter f [a1; ...; an]] applies function [f] in turn to [[a1; ...; an]]. it is equivalent to [f a1; f a2; ...; f an]*)

val iteri : f:(int -> 'a -> unit) -> 'a t -> unit
(**Same as {!val-iter}, but the function is applied to the index of the element as first argument (counting from 0), and the element itself as second argument*)

val fold : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b
(** Fold on the list's elements. *)

val fold_rev : f:('b -> 'a -> 'b) -> x:'b -> 'a t -> 'b
(** Fold on the list's elements, in reverse order (starting from the tail). *)

val rev_map : f:('a -> 'b) -> 'a t -> 'b t
(** [rev_map f l] is the same as [map f (rev l)], but is more efficient.*)

val rev : 'a t -> 'a t
(** Reverse the list. *)

val equal : eq:('a -> 'a -> bool) -> 'a t -> 'a t -> bool
(**[equal eq [a1; ...; an] [b1; ...; bm]] holds when the two input lists have the same length, and for each pair of elements [ai] [bi] at the same position we have [eq ai bi]*)

val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int
(** Lexicographic comparison. 
    [compare cmp [a1; ...; an] [b1; ...; bn]] performs a lexicographic comparison of the two input lists, using the same ['a -> 'a -> int] interface as [compare].*)


(** {2 Utils} *)

val make : int -> 'a -> 'a t
(** [make n v] creates a list of length [n] with all elements initialized to [v].*)

val repeat : int -> 'a t -> 'a t
(** [repeat n l] is [append l (append l ... l)] [n] times. 

    @raise Invalid_argument if [n] is negative 
*)

val range : int -> int -> int t
(** [range i j] is [i; i+1; ... ; j] or [j; j-1; ...; i]. *)

(** {2 Conversions} *)

type 'a iter = ('a -> unit) -> unit
(** An iterator type, representing a collection by its fold behavior.*)

type 'a gen = unit -> 'a option
(** A generator function that returns [Some value] or [None] when exhausted.*)

val add_list : 'a t -> 'a list -> 'a t
(** Append a standard list to the given RAL.*)

val of_list : 'a list -> 'a t
(** Convert a list to a RAL.*)

val to_list : 'a t -> 'a list
(** Convert a RAL to a standard list.
*)

val of_list_map : f:('a -> 'b) -> 'a list -> 'b t
(** Combination of {!of_list} and {!map}. *)

val of_array : 'a array -> 'a t
(** Convert an array to a list.*)

val add_array : 'a t -> 'a array -> 'a t
(** Append an array to the given list.*)

val to_array : 'a t -> 'a array
(** Convert a RAL to an array. More efficient than on usual lists. *)

val add_iter : 'a t -> 'a iter -> 'a t
(**Consume an iterator and append its elements to the given list.*)

val of_iter : 'a iter -> 'a t
(**Build a list from an iterator.*)

val to_iter : 'a t -> 'a iter
(** Create an iterator from a list.*)

val add_gen : 'a t -> 'a gen -> 'a t
(** Consume a generator and append its elements to the given list.*)

val of_gen : 'a gen -> 'a t
(** Build a list from a generator. *)

val to_gen : 'a t -> 'a gen
(** Create a generator from a list. *)


(** {2 Infix} *)

module Infix : sig
  val ( @+ ) : 'a -> 'a t -> 'a t
  (** Cons (alias to {!cons}). *)

  val ( >>= ) : 'a t -> ('a -> 'b t) -> 'b t
  (** Alias to {!flat_map}. *)

  val ( >|= ) : 'a t -> ('a -> 'b) -> 'b t
  (** Alias to {!map}. *)

  val ( <*> ) : ('a -> 'b) t -> 'a t -> 'b t
  (** Alias to {!app}. *)

  val ( -- ) : int -> int -> int t
  (** Alias to {!range}. *)

  val ( --^ ) : int -> int -> int t
  (** [a --^ b] is the integer range from [a] to [b], where [b] is excluded.
      @since 0.17 *)
end

include module type of Infix


(** {2 IO} *)

type 'a printer = Format.formatter -> 'a -> unit

val pp : ?pp_sep:unit printer -> 'a printer -> 'a t printer
(** Print a list.*)
