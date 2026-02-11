type t =
  { lines_above: string list
  ; current_line: Uchar.t Zipper.t
  ; lines_below: string list
  ; curswant: int }

let empty_buffer =
  {lines_above= []; current_line= Zipper.empty; lines_below= []; curswant= 0}

(* helper to apply function n times onto some value *)
let rec apply_n n f x = if n <= 0 then x else apply_n (n - 1) f (f x)

let uchars_to_string (uchars : Uchar.t list) =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars ;
  Buffer.contents buf

let string_to_uchars str =
  let len = String.length str in
  let rec loop i acc =
    if i >= len then acc
    else
      let decoded = String.get_utf_8_uchar str i in
      let u = Uchar.utf_decode_uchar decoded in
      let char_len = Uchar.utf_decode_length decoded in
      loop (i + char_len) (u :: acc)
  in
  loop 0 [] |> List.rev

let cursor_coords buffer =
  let x = Zipper.position buffer.current_line in
  let y = List.length buffer.lines_above in
  (x, y)

let insert_uchar uchar buffer =
  let new_line = Zipper.insert uchar buffer.current_line in
  {buffer with current_line= new_line; curswant= Zipper.position new_line}

let backspace buffer =
  if not (Zipper.is_start buffer.current_line) then
    {buffer with current_line= Zipper.delete_left buffer.current_line}
  else
    match buffer.lines_above with
    | [] ->
        buffer
    | prev_line_str :: rest_above ->
        let prev_chars = string_to_uchars prev_line_str in
        let new_line = Zipper.prepend_left prev_chars buffer.current_line in
        { buffer with
          lines_above= rest_above
        ; current_line= new_line
        ; curswant= Zipper.position new_line }

let newline buffer =
  let prev, next = buffer.current_line |> Zipper.split in
  let old_line = Zipper.to_list prev |> uchars_to_string in
  { buffer with
    lines_above= old_line :: buffer.lines_above
  ; current_line= next
  ; curswant= 0 }

let del_forward buffer =
  let deleted = Zipper.focus buffer.current_line in
  let current_line = Zipper.delete_right buffer.current_line in
  ({buffer with current_line; curswant= Zipper.position current_line}, deleted)

let del_backward buffer =
  let deleted = Zipper.focus_left buffer.current_line in
  let current_line = Zipper.delete_left buffer.current_line in
  ({buffer with current_line; curswant= Zipper.position current_line}, deleted)

let insert_line dir buffer =
  match dir with
  | `Below ->
      let old_line =
        buffer.current_line |> Zipper.to_list |> uchars_to_string
      in
      { buffer with
        lines_above= old_line :: buffer.lines_above
      ; current_line= Zipper.empty }
  | `Above ->
      let old_line =
        buffer.current_line |> Zipper.to_list |> uchars_to_string
      in
      { buffer with
        lines_below= old_line :: buffer.lines_below
      ; current_line= Zipper.empty }

let delete_line buffer =
  let buffer =
    match (buffer.lines_below, buffer.lines_above) with
    | next :: lines_below, _ ->
        let current_line = next |> string_to_uchars |> Zipper.of_list in
        {buffer with current_line; lines_below}
    | [], prev :: lines_above ->
        let current_line = prev |> string_to_uchars |> Zipper.of_list in
        {buffer with current_line; lines_above}
    | [], [] ->
        {buffer with current_line= Zipper.empty}
  in
  let current_line =
    apply_n buffer.curswant Zipper.move_right buffer.current_line
  in
  {buffer with current_line}

let reset_line buffer = {buffer with current_line= Zipper.empty}

type cursor_target = Start | Curswant | End

(* helper to swap current line with target line string *)
let swap_line target_str target buffer =
  let z = target_str |> string_to_uchars |> Zipper.of_list in
  let target_zipper =
    match target with
    | Start ->
        z
    | Curswant ->
        apply_n buffer.curswant Zipper.move_right z
    | End ->
        Zipper.move_end z
  in
  let old_line = buffer.current_line |> Zipper.to_list |> uchars_to_string in
  (old_line, target_zipper)

(* we use polymorphic variants to match with Notty `Arrow type *)
(* this function will not move to the next/previous line - no wrapping *)
let move dir buffer =
  match dir with
  | `Left ->
      let current_line = Zipper.move_left buffer.current_line in
      let curswant = Zipper.position current_line in
      {buffer with current_line; curswant}
  | `Right ->
      let current_line = Zipper.move_right buffer.current_line in
      let curswant = Zipper.position current_line in
      {buffer with current_line; curswant}
  | `Up -> (
    match buffer.lines_above with
    | [] ->
        buffer
    | prev :: lines_above ->
        let old, current_line = swap_line prev Curswant buffer in
        { buffer with
          lines_above
        ; current_line
        ; lines_below= old :: buffer.lines_below } )
  | `Down -> (
    match buffer.lines_below with
    | [] ->
        buffer
    | next :: lines_below ->
        let old, current_line = swap_line next Curswant buffer in
        { buffer with
          lines_above= old :: buffer.lines_above
        ; current_line
        ; lines_below } )
  | `End ->
      let curswant = Zipper.position buffer.current_line in
      let current_line = Zipper.move_end buffer.current_line in
      {buffer with current_line; curswant}
  | `Start ->
      let curswant = Zipper.position buffer.current_line in
      let current_line = Zipper.move_start buffer.current_line in
      {buffer with current_line; curswant}

(* this function can move to next line *)
let step_forward buffer =
  if not (Zipper.is_end buffer.current_line) then
    Some {buffer with current_line= Zipper.move_right buffer.current_line}
  else
    match buffer.lines_below with
    | [] ->
        None
    | next :: lines_below ->
        let old, current_line = swap_line next Start buffer in
        Some
          { buffer with
            lines_above= old :: buffer.lines_above
          ; current_line
          ; lines_below }

(* this function can move to previous line *)
let step_backward buffer =
  if not (Zipper.is_start buffer.current_line) then
    Some {buffer with current_line= Zipper.move_left buffer.current_line}
  else
    match buffer.lines_above with
    | [] ->
        None
    | prev :: lines_above ->
        let old, current_line = swap_line prev End buffer in
        Some
          { buffer with
            lines_above
          ; current_line
          ; lines_below= old :: buffer.lines_below }

let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '@' ->
      true
  | _ ->
      false

type char_class = Word | Space | Other

(* classify uchar based on the word style *)
let get_class style u : char_class =
  if Uchar.is_char u then
    match (style, Uchar.to_char u) with
    | _, (' ' | '\t') ->
        Space
    | `WORD, _ ->
        Word
    | `Word, c when is_alphanum c ->
        Word
    | `Word, _ ->
        Other
  else Word

let rec skip_while p skip_func acc acc_func buf =
  if p buf then
    match skip_func buf with
    | Some (next_buffer, skipped) ->
        let new_acc = acc_func acc skipped in
        skip_while p skip_func new_acc acc_func next_buffer
    | None ->
        (buf, acc)
  else (buf, acc)

let accumulate = fun acc x -> x :: acc

let only_last = fun _acc x -> x

let step end_func peek_func move_func buf =
  if not (end_func buf.current_line) then
    Some
      ( {buf with current_line= move_func buf.current_line}
      , buf.current_line |> peek_func |> Option.get )
  else None

let peek_forward = Zipper.focus

let peek_backward = Zipper.focus_left

let step_forward = step Zipper.is_end peek_forward Zipper.move_right

let step_backward = step Zipper.is_start peek_backward Zipper.move_left

let step_forward_del = step Zipper.is_end peek_forward Zipper.delete_right

let step_backward_del = step Zipper.is_start peek_backward Zipper.delete_left

let parse_till_start classify_func peek_func step_func (buf : t) : t * string =
  (* check what is the class of next value in buffer *)
  (* this treats end of line as a Space *)
  let classify b =
    b.current_line |> peek_func |> Option.map classify_func
    |> Option.value ~default:Space
  in
  let is_wordclass wc b = classify b = wc in
  let first_char_class = classify buf in
  let buf, skipped =
    skip_while (is_wordclass first_char_class) step_func [] accumulate buf
  in
  let buf, skipped =
    skip_while (is_wordclass Space) step_func skipped accumulate buf
  in
  (buf, uchars_to_string skipped)

let parse_till_end classify_func peek_func step_func (buf : t) : t * string =
  let classify b =
    b.current_line |> peek_func |> Option.map classify_func
    |> Option.value ~default:Space
  in
  let is_wordclass wc b = classify b = wc in
  let buf, skipped =
    skip_while (is_wordclass Space) step_func [] accumulate buf
  in
  let first_non_space_class = classify buf in
  let buf, skipped =
    skip_while
      (is_wordclass first_non_space_class)
      step_func skipped accumulate buf
  in
  (buf, uchars_to_string skipped)

let string_rev s = s |> string_to_uchars |> List.rev |> uchars_to_string
