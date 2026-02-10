type state

(** Abstract data type of Parser. *)
type t = Continue of state | Finished of Command.t | Fail [@@deriving show]

val init : t
(** Parser with initial state. *)

val step : t -> char -> t
(** Update Parser state machine with new char. *)
