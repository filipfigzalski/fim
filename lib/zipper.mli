type 'a t

val empty : 'a t
val of_list : 'a list -> 'a t
val to_list : 'a t -> 'a list
val is_empty : 'a t -> bool
val is_start : 'a t -> bool
val position : 'a t -> int
val move_left : 'a t -> 'a t
val move_right : 'a t -> 'a t
val insert : 'a -> 'a t -> 'a t
val delete_left : 'a t -> 'a t
val prepend_left : 'a list -> 'a t -> 'a t
