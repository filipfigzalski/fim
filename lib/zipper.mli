(** A list zipper data structure. *)

(** Abstract data type of zipper. *)
type 'a t

val empty : 'a t
(** An empty zipper. *)

val of_list : 'a list -> 'a t
(** Create zipper from list.
    Cursor is set at the start.
 *)

val to_list : 'a t -> 'a list
(** Collapse zipper into list. *)

val is_empty : 'a t -> bool
(** [is_empty z] is true if and only if [z] has no elements. *)

val is_start : 'a t -> bool
(** [is_start z] is true if and only if [z] cursor is at the start of zipper.*)

val is_end : 'a t -> bool
(** [is_end z] is true if and only if [z] cursor is at the end of zipper.*)

val position : 'a t -> int
(** [position z] returns index of current focus. *)

val focus : 'a t -> 'a option
(** [focus z] returns [Some x] where [x] is current element under the cursor,
    or [None] if cursor is at the end of zipper.
*)

val focus_left : 'a t -> 'a option
(** [focus_left z] returns [Some x] where [x] is the element to the left of 
    current cursor position, or [None] if cursor is at the start of zipper.
*)

val move_left : 'a t -> 'a t
(** [move_left z] moves the cursor one position to the left.
    If the focus is already at the start, returns unchanged zipper.
*)

val move_right : 'a t -> 'a t
(** [move_right z] moves the cursor one position to the right.
    If the focus is already at the end, returns unchanged zipper.
*)

val move_start : 'a t -> 'a t
(** [move_start z] moves the cursor to the start of the zipper. *)

val move_end : 'a t -> 'a t
(** [move_end z] moves the cursor to the end of the zipper. *)

val insert : 'a -> 'a t -> 'a t
(** [insert x z] inserts element [x] at the current cursor position. *)

val prepend_left : 'a list -> 'a t -> 'a t
(** [prepend_left l z] inserts the entire list [l] to the left of current
    cursor position.
*)

val delete_left : 'a t -> 'a t
(** [delete_left z] removes the element to the left of the cursor.*)

val delete_right : 'a t -> 'a t
(** [delete_right z] removes the element to the right of the cursor.*)

val split : 'a t -> 'a t * 'a t
(** [split z] splits the zipper into two new zippers at the current focus.
    @returns [(left_part, right_part)]
*)
