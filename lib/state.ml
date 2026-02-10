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

let execute_command (c : Command.t) (s : t) : t = {s with last_cmd= Some c}

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
    | _ ->
        s )
