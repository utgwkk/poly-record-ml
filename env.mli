type 'a t

exception Not_bound of Syntax.id

val empty : 'a t

val extend : Syntax.id -> 'a -> 'a t -> 'a t

val lookup : Syntax.id -> 'a t -> 'a
