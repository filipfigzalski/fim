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

let get_buffer (state : State.t) = state.buffer

let set_cursor_shape (state : State.t) =
  match state.mode with
  | Insert ->
      Cursor_shape.bar_blinking ()
  | Normal ->
      Cursor_shape.block_steady ()

let rec loop term state =
  Term.image term (Ui.render state) ;
  (* position cursor *)
  let buffer = get_buffer state in
  let col, row = Text_buffer.cursor_coords buffer in
  Term.cursor term (Some (col, row)) ;
  set_cursor_shape state ;
  match Term.event term with
  (* TODO: this is workaround before I implement command mode *)
  | `Key (`ASCII 'Q', mods) when List.mem `Ctrl mods ->
      ()
  | `End ->
      ()
  | `Key key ->
      let new_state = State.handle_input state key in
      loop term new_state
  | _ ->
      loop term state

let () =
  let term = Term.create () in
  loop term State.empty
