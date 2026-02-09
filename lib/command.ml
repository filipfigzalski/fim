type operator = Delete | Change | Yank [@@deriving show]

type immediate =
  | Paste of [`Before | `After]
  | DeleteChar of [`Before | `After]
  | Undo
  | Redo
[@@deriving show]

type mode_switch = Insert | Append | Open of [`Below | `Above]
[@@deriving show]

type t =
  | Operation of {count: int; op: operator; target: Motion.t}
  | Navigation of {count: int; move: Move.t}
  | Action of {count: int; action: immediate}
  | Switch of mode_switch
[@@deriving show]

let immediate_of_char = function
  | 'x' ->
      Some (DeleteChar `Before)
  | 'X' ->
      Some (DeleteChar `After)
  | 'p' ->
      Some (Paste `Before)
  | 'P' ->
      Some (Paste `After)
  | 'u' ->
      Some Undo
  (* <Ctrl-R> *)
  | '\018' ->
      Some Redo
  | _ ->
      None

let switch_of_char = function
  | 'a' ->
      Some Append
  | 'i' ->
      Some Insert
  | 'o' ->
      Some (Open `Below)
  | 'O' ->
      Some (Open `Above)
  | _ ->
      None

let operation_of_char = function
  | 'd' ->
      Some Delete
  | 'c' ->
      Some Change
  | 'y' ->
      Some Yank
  | _ ->
      None
