open Types

let initial = { mode = Normal; buffer = Text_buffer.empty_buffer; curswant = 0 }

let handle_input state input =
  match (state.mode, input) with
  | _, `Key (`Backspace, _) ->
      let new_buffer = Text_buffer.backspace state.buffer in
      { state with buffer = new_buffer }
  | _, `Key (`Enter, _) ->
      let new_buffer = Text_buffer.newline state.buffer in
      { state with buffer = new_buffer }
  | _, `Key (`ASCII c, _) ->
      let new_buffer =
        Text_buffer.insert_uchar (Uchar.of_char c) state.buffer
      in
      { state with buffer = new_buffer }
  | _, `Key (`Uchar u, _) ->
      let new_buffer = Text_buffer.insert_uchar u state.buffer in
      { state with buffer = new_buffer }
  | _ -> state

let initial = { mode = Normal; buffer = Text_buffer.empty_buffer; curswant = 0 }
