(* .mli Improved Myers List*)

type 'a cell
type 'a t

val empty : 'a t
(** Empty list*)

val is_empty : 'a t -> bool
(** Check whether the list is empty*)

val cons : 'a -> 'a t -> 'a t

val hd : 'a t -> 'a

(*val tl : 'a t -> 'a t - acho que esta função não faz sentido para esta estrutura*)

val return : 'a -> 'a t
(** Singleton *)

val length : 'a t -> int 
(** Number of elements*)
