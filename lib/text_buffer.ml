type t =
  { lines_above: string list
  ; current_line: Uchar.t Zipper.t
  ; lines_below: string list
  ; curswant: int }

let empty_buffer =
  {lines_above= []; current_line= Zipper.empty; lines_below= []; curswant= 0}

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

(* helper to apply function n times onto some value *)
let rec apply_n n f x = if n <= 0 then x else apply_n (n - 1) f (f x)

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
      {buffer with current_line= Zipper.move_end buffer.current_line}
  | `Start ->
      {buffer with current_line= Zipper.move_start buffer.current_line}

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

type word_style = Word | WORD

type word_direction = Forward | Backward

type word_part = End | Start

(* classify uchar based on the word style *)
let get_class style u : char_class =
  if Uchar.is_char u then
    match (style, Uchar.to_char u) with
    | _, (' ' | '\t') ->
        Space
    | WORD, _ ->
        Word
    | Word, c when is_alphanum c ->
        Word
    | Word, _ ->
        Other
  else Word

(* helper to repeat a step function while condition holds. *)
let rec skip_while p f buffer =
  if p buffer then
    match f buffer with
    | None ->
        buffer
    | Some next_buffer ->
        skip_while p f next_buffer
  else buffer

let move_word style direction trajectory buffer : t =
  let next_char b =
    match direction with
    | Forward ->
        Zipper.focus b.current_line
    | Backward ->
        Zipper.focus_left b.current_line
  in
  (* function that takes current Text_buffer state + word_style and returns
      char_class at current position
   *)
  let current_cls b =
    next_char b |> Option.map (get_class style) |> Option.value ~default:Space
  in
  (* pick right step function *)
  let step, rev_step =
    match direction with
    | Forward ->
        (step_forward, step_backward)
    | Backward ->
        (step_backward, step_forward)
  in
  let buffer =
    match (direction, trajectory) with
    | Forward, End ->
        buffer |> step |> Option.value ~default:buffer
    | _ ->
        buffer
  in
  let buffer =
    match trajectory with
    | Start ->
        (* if we start at Word/Other, we must go till the end of it *)
        let start_type = current_cls buffer in
        let buffer =
          if start_type = Space then buffer
          else skip_while (fun u -> current_cls u = start_type) step buffer
        in
        (* if we are on space we also skip it *)
        let buffer = skip_while (fun u -> current_cls u = Space) step buffer in
        buffer
    | End ->
        let start_type = current_cls buffer in
        let buffer =
          if start_type != Space then buffer
          else skip_while (fun u -> current_cls u = Space) step buffer
        in
        let word_type = current_cls buffer in
        let buffer =
          skip_while (fun u -> current_cls u = word_type) step buffer
        in
        buffer
  in
  let buffer =
    match (direction, trajectory) with
    | Forward, End ->
        buffer |> rev_step |> Option.value ~default:buffer
    | Backward, Start ->
        buffer |> step |> Option.value ~default:buffer
    | _ ->
        buffer
  in
  {buffer with curswant= Zipper.position buffer.current_line}
