type mode = Normal | Insert | Command

type t = {mode: mode; buffer: Text_buffer.t}

let initial = {mode= Insert; buffer= Text_buffer.empty_buffer}

let move_dir_of_char = function
  | 'h' ->
      Some `Left
  | 'j' ->
      Some `Down
  | 'k' ->
      Some `Up
  | 'l' ->
      Some `Right
  | _ ->
      None

let handle_input state input =
  match (state.mode, input) with
  | _, `Key (`Backspace, _) ->
      {state with buffer= Text_buffer.backspace state.buffer}
  | Insert, `Key (`Enter, _) ->
      {state with buffer= Text_buffer.newline state.buffer}
  | Normal, `Key (`Enter, _) ->
      {state with buffer= Text_buffer.move `Down state.buffer}
  | (Normal | Insert), `Key (`Arrow a, _) ->
      {state with buffer= Text_buffer.move a state.buffer}
  | Insert, `Key (`ASCII c, []) ->
      let buffer = Text_buffer.insert_uchar (Uchar.of_char c) state.buffer in
      {state with buffer}
  | Insert, `Key (`Uchar u, _) ->
      let buffer = Text_buffer.insert_uchar u state.buffer in
      {state with buffer}
  | Normal, `Key (`ASCII 'i', []) ->
      {state with mode= Insert}
  | Normal, `Key (`ASCII c, []) when Option.is_some (move_dir_of_char c) ->
      let dir = Option.get (move_dir_of_char c) in
      {state with buffer= Text_buffer.move dir state.buffer}
  | Insert, `Key (`Escape, _) ->
      {state with mode= Normal}
  | _ ->
      state
