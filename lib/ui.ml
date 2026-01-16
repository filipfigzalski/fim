open Notty

let uchars_to_string (uchars : Uchar.t list) =
  let buf = Buffer.create (List.length uchars) in
  List.iter (Buffer.add_utf_8_uchar buf) uchars;
  Buffer.contents buf

let draw_line str = I.string A.empty str

let render (state : State.t) =
  let above_imgs = state.buffer.lines_above |> List.rev |> List.map draw_line in
  let current_content =
    Zipper.to_list state.buffer.current_line |> Text_buffer.uchars_to_string
  in
  let current_img = draw_line current_content in
  let below_imgs = state.buffer.lines_below |> List.map draw_line in
  I.vcat (above_imgs @ [ current_img ] @ below_imgs)
