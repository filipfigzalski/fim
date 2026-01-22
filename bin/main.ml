open Notty_unix
open Fim

module Cursor_shape = struct
  let fmt = Printf.printf "\027[%d q%!"
  let block_blinking () = fmt 1
  let block_steady () = fmt 2
  let underline_blinking () = fmt 3
  let underline_steady () = fmt 4
  let bar_blinking () = fmt 5
  let bar_steady () = fmt 6
end

let rec loop term state =
  Term.image term (Ui.render state);

  let col, row = Text_buffer.cursor_coords state.buffer in
  Term.cursor term (Some (col, row));

  (match state.mode with
  | Insert -> Cursor_shape.bar_steady ()
  | Normal -> Cursor_shape.block_steady ()
  | _ -> Cursor_shape.block_steady ());

  match Term.event term with
  | `Key (`Escape, _) -> ()
  | `End -> ()
  | `Key _ as key ->
      let new_state = State.handle_input state key in
      loop term new_state
  | _ -> loop term state

let () =
  let term = Term.create () in
  loop term State.initial
