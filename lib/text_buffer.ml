open Types

let empty_buffer =
  { lines_above = []; current_line = Zipper.empty; lines_below = [] }

let uchars_to_string (uchars : Uchar.t list) =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars;
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
  { buffer with current_line = new_line }

let backspace buffer =
  if not (Zipper.is_start buffer.current_line) then
    { buffer with current_line = Zipper.delete_left buffer.current_line }
  else
    match buffer.lines_above with
    | [] -> buffer
    | prev_line_str :: rest_above ->
        let prev_chars = string_to_uchars prev_line_str in
        let new_zipper = Zipper.prepend_left prev_chars buffer.current_line in
        { buffer with lines_above = rest_above; current_line = new_zipper }

let newline buffer =
  let old_line = buffer.current_line |> Zipper.to_list |> uchars_to_string in
  {
    buffer with
    lines_above = old_line :: buffer.lines_above;
    current_line = Zipper.empty;
  }
