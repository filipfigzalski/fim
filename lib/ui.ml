open Notty

let uchars_to_string (uchars : Uchar.t list) =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars ;
  Buffer.contents buf

let draw_line str = I.string A.empty str

let draw_multiline attr s =
  s |> String.split_on_char '\n'
  |> List.map (fun line -> I.string attr line)
  |> I.vcat

let render (state : State.t) =
  let above_imgs = state.buffer.lines_above |> List.rev |> List.map draw_line in
  let current_content =
    Zipper.to_list state.buffer.current_line |> Text_buffer.uchars_to_string
  in
  let current_img = draw_line current_content in
  let below_imgs = state.buffer.lines_below |> List.map draw_line in
  let debug_str =
    "Parser state: \n" ^ Parser.show state.parser ^ "\n" ^ "Last CMD: \n"
    ^ match state.last_cmd with Some cmd -> Command.show cmd | None -> ""
  in
  let debug_img = draw_multiline A.(fg lightyellow) debug_str in
  I.vcat ([debug_img] @ above_imgs @ [current_img] @ below_imgs)
