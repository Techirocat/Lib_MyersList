(* .mli Improved Myers List*)

type 'a cell
type 'a t

val empty : 'a t
(** Empty list*)

val is_empty : 'a t -> bool
(** Check whether the list is empty*)


