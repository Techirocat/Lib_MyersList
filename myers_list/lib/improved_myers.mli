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

val get : 'a t -> int -> 'a option
(** O(log(n))*)

val get_exn : 'a t -> int -> 'a 
(** O(log(n))*)

(* val set : 'a t -> int -> 'a -> 'a t 
-> não estou a ver como implementar de forma eficiente, apenas O(i)
*)

(* val remove : 'a t -> int -> 'a t 
-> não estou a ver como implementar de forma eficiente
*)

(* val get_and_remove_exn : 'a t -> int -> 'a * 'a t*)

val map : ('a -> 'b) -> 'a t -> 'b t

val to_list_rev : 'a t -> 'a list
    
val to_list : 'a t -> 'a list 

val to_list_map : 'a t -> ('a -> 'b) -> 'b list

val add_list : 'a t -> 'a list -> 'a t

val of_list : 'a list -> 'a t

val of_list_map : ('a -> 'b) -> 'a list -> 'b t


val make : int -> 'a -> 'a t

val append : 'a t -> 'a t -> 'a t
(** O(m) - onde m é o numero de elmetos da segunda lista*)

val repeat : int -> 'a t -> 'a t
(** repeat n l is append l (append l ... l) n times.*) 

val range : int -> int -> int t
(** range i j is i; i+1; ... ; j or j; j-1; ...; i.*)

val equal : ('a -> 'a -> bool) -> 'a t -> 'a t -> bool




val mapi : (int -> 'a -> 'b) -> 'a t -> 'b t


