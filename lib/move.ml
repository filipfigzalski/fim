type direction = [`Forward | `Backward] [@@deriving show]

type t =
  | Char of direction
  | Line of direction
  | Word of {style: [`Word | `WORD]; part: [`End | `Start]; dir: direction}
[@@deriving show]

let of_char = function
  (* basic directions *)
  | 'h' ->
      Some (Char `Backward)
  | 'j' ->
      Some (Line `Forward)
  | 'k' ->
      Some (Line `Backward)
  | 'l' ->
      Some (Char `Forward)
  (* word motions *)
  | 'w' ->
      Some (Word {style= `Word; part= `Start; dir= `Forward})
  | 'W' ->
      Some (Word {style= `WORD; part= `Start; dir= `Forward})
  | 'b' ->
      Some (Word {style= `Word; part= `End; dir= `Backward})
  | 'B' ->
      Some (Word {style= `WORD; part= `End; dir= `Backward})
  | 'e' ->
      Some (Word {style= `Word; part= `End; dir= `Forward})
  | 'E' ->
      Some (Word {style= `WORD; part= `End; dir= `Forward})
  | _ ->
      None
