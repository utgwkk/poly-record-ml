type ('a, 'b) t

exception Not_bound

val empty : ('a, 'b) t
val extend : 'a -> 'b -> ('a, 'b) t -> ('a, 'b) t
val lookup : 'a -> ('a, 'b) t -> 'b
val lookup_opt : 'a -> ('a, 'b) t -> 'b option
val map : ('b -> 'c) -> ('a, 'b) t -> ('a, 'c) t
val remove : 'a -> ('a, 'b) t -> ('a, 'b) t
val fold_right : ('b -> 'c -> 'c) -> ('a, 'b) t -> 'c -> 'c

val from_list : ('a * 'b) list -> ('a, 'b) t
val domain : ('a, 'b) t -> 'a list
