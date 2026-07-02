(* .mli Improved Myers List*)

(*
Funções em falta

val tl : 'a t -> 'a t

val front : 'a t -> ('a * 'a t) option

val front_exn : 'a t -> 'a * 'a t

val set : 'a t -> int -> 'a -> 'a t

val remove : 'a t -> int -> 'a t

val get_and_remove_exn : 'a t -> int -> 'a * 'a t

val flatten : 'a t t -> 'a t

val app : ('a -> 'b) t -> 'a t -> 'b t

val compare : cmp:('a -> 'a -> int) -> 'a t -> 'a t -> int

Os operados de INFIX e o IO

*)

(*
TODO:

Funções para verificar se é possivel implementar usando fold e fold_rev

rev_map
rev
append
filter
to_list

*)

(*
TODO:
- Implementar funções com fold e fold_rev
- Implementar funções em falta
- Criar testes e testar todas as Funções
- ocamldoc
*)

type 'a cell
type 'a t

type 'a iter = ('a -> unit) -> unit
type 'a gen = unit -> 'a option

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

(* val set : 'a t -> int -> 'a -> 'a t *) 

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


val filter : ('a -> bool) -> 'a t -> 'a t

val iter : ('a -> unit) -> 'a t -> unit

val iteri : (int -> 'a -> unit) -> 'a t -> unit

val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b

val fold_rev : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
(* A implementação que eu fiz não é tail recursive, pode ser  preciso alterar*)

val rev : 'a t -> 'a t

val rev_map : ('a -> 'b) -> 'a t -> 'b t

val of_array : 'a array -> 'a t

val add_array : 'a t -> 'a array -> 'a t

val to_array : 'a t -> 'a array

val filter_map : ('a -> 'b option) -> 'a t -> 'b t

val flat_map : ('a -> 'b t) -> 'a t -> 'b t

val take : int -> 'a t -> 'a t

val take_while : ('a -> bool) -> 'a t -> 'a t

val drop : int -> 'a t -> 'a t

val drop_while : ('a -> bool) -> 'a t -> 'a t


val take_drop : int -> 'a t -> 'a t * 'a t
(* take_drop n l splits l into a, b such that length a = n if length l >= n, and such that append a b = l. *)

val add_iter : 'a t -> 'a iter -> 'a t

val of_iter : 'a iter -> 'a t

val to_iter : 'a t -> 'a iter

val add_gen : 'a t -> 'a gen -> 'a t

val of_gen : 'a gen -> 'a t

val to_gen : 'a t -> 'a gen