open Notty_unix
open Fim

let rec loop term state =
  Term.image term (Ui.render state);

  let col, row = Text_buffer.cursor_coords state.buffer in
  Term.cursor term (Some (col, row));

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
