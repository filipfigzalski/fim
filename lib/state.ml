type mode = Normal | Insert

type t =
  { buffer: Text_buffer.t
  ; mode: mode
  ; parser: Parser.t
  ; last_cmd: Command.t option }

let empty : t =
  { buffer= Text_buffer.empty_buffer
  ; mode= Normal
  ; parser= Parser.init
  ; last_cmd= None }

let rec apply_n n f x = if n <= 0 then x else apply_n (n - 1) f (f x)

let execute_command (c : Command.t) (s : t) : t =
  let s =
    match c with
    | Command.Switch Insert ->
        {s with mode= Insert}
    | Command.Navigation {count; move} ->
        let cmd =
          match move with
          | Move.Char `Forward ->
              Text_buffer.move `Right
          | Move.Char `Backward ->
              Text_buffer.move `Left
          | Move.Line `Forward ->
              Text_buffer.move `Down
          | Move.Line `Backward ->
              Text_buffer.move `Up
          | Move.Word {style; part; dir} ->
              Navigation.move_word style dir part
        in
        let buffer = apply_n count cmd s.buffer in
        {s with buffer}
    | _ ->
        s
  in
  {s with last_cmd= Some c}

let handle_input (s : t) (k : Notty.Unescape.key) : t =
  match s.mode with
  | Normal -> (
    match k with
    (* Escape should always reset parser *)
    | `Escape, _ ->
        {s with parser= Parser.init}
    | `ASCII c, _ -> (
        let parser = Parser.step s.parser c in
        match parser with
        | Parser.Continue _ ->
            {s with parser}
        | Parser.Finished c ->
            let s = execute_command c s in
            {s with parser= Parser.init}
        | Parser.Fail ->
            {s with parser= Parser.init} )
    | _ ->
        s )
  | Insert -> (
    match k with
    | `Escape, _ ->
        {s with mode= Normal}
    | `ASCII 'C', m when List.mem `Ctrl m ->
        {s with mode= Normal}
    | `ASCII c, _ ->
        {s with buffer= Text_buffer.insert_uchar (Uchar.of_char c) s.buffer}
    | `Uchar u, _ ->
        {s with buffer= Text_buffer.insert_uchar u s.buffer}
    | `Enter, _ ->
        {s with buffer= Text_buffer.newline s.buffer}
    | `Backspace, _ ->
        {s with buffer= Text_buffer.backspace s.buffer}
    | `Arrow a, _ ->
        {s with buffer= Text_buffer.move a s.buffer}
    | _ ->
        s )
