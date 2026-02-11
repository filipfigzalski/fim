type textobject = WordObject of [`Word | `WORD] [@@deriving show]

type scope = Inner | Around [@@deriving show]

type t =
  | Move of {count: int; move: Move.t}
  | TextObject of {count: int; scope: scope; obj: textobject}
  | Line
[@@deriving show]

let textobject_of_char = function
  | 'w' ->
      Some (WordObject `Word)
  | 'W' ->
      Some (WordObject `WORD)
  | _ ->
      None

let scope_of_char = function
  | 'i' ->
      Some Inner
  | 'a' ->
      Some Around
  | _ ->
      None
