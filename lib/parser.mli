type state

type t = Continue of state | Finished of Command.t | Fail [@@deriving show]

val init : t
(** The initial stat *)

val reset : t -> t

val step : t -> char -> t
