type mode = Normal | Insert

type t =
  { buffer: Text_buffer.t
  ; mode: mode
  ; parser: Parser.t
  ; undo_stack: Text_buffer.t list
  ; redo_stack: Text_buffer.t list
  ; last_cmd: Command.t option }

let empty : t =
  { buffer= Text_buffer.empty_buffer
  ; mode= Normal
  ; parser= Parser.init
  ; undo_stack= []
  ; redo_stack= []
  ; last_cmd= None }

let rec apply_n n f x = if n <= 0 then x else apply_n (n - 1) f (f x)

let execute_command (c : Command.t) (s : t) : t =
  match c with
  | Switch Insert ->
      {s with mode= Insert; undo_stack= s.buffer :: s.undo_stack; redo_stack= []}
  | Switch Append ->
      { s with
        buffer= Text_buffer.move `Right s.buffer
      ; mode= Insert
      ; undo_stack= s.buffer :: s.undo_stack
      ; redo_stack= [] }
  | Switch (Open dir) ->
      { s with
        mode= Insert
      ; buffer= Text_buffer.insert_line dir s.buffer
      ; undo_stack= s.buffer :: s.undo_stack
      ; redo_stack= [] }
  | Action {action= Undo; count} -> (
    (* TODO: implement count *)
    match s.undo_stack with
    | [] ->
        s
    | x :: xs ->
        {s with buffer= x; undo_stack= xs; redo_stack= s.buffer :: s.redo_stack}
    )
  | Action {action= Redo; count} -> (
    (* TODO: implement count *)
    match s.redo_stack with
    | [] ->
        s
    | x :: xs ->
        {s with buffer= x; redo_stack= xs; undo_stack= s.buffer :: s.undo_stack}
    )
  | Action {action= Paste dir; count} ->
      failwith "paste not implemented"
  | Action {action= DeleteChar dir; count} ->
      (* TODO: apply count *)
      let del_cmd =
        match dir with
        | `Before ->
            Text_buffer.del_backward
        | `After ->
            Text_buffer.del_forward
      in
      let buffer, deleted = del_cmd s.buffer in
      {s with buffer; undo_stack= s.buffer :: s.undo_stack; redo_stack= []}
  | Operation {op= Delete; target= Line; count} ->
      let buffer = apply_n count Text_buffer.delete_line s.buffer in
      {s with buffer; undo_stack= s.buffer :: s.undo_stack; redo_stack= []}
  | Operation {op= Change; target= Line; count} ->
      { s with
        buffer=
          s.buffer
          |> apply_n (count - 1) Text_buffer.delete_line
          |> Text_buffer.reset_line
      ; mode= Insert
      ; undo_stack= s.buffer :: s.undo_stack
      ; redo_stack= [] }
  | Operation {op= Delete; target= Move {move= Line dir; count= c1}; count= c2}
    ->
      let count = (c1 * c2) + 1 in
      let new_buffer =
        match dir with
        | `Forward ->
            s.buffer |> apply_n count Text_buffer.delete_line
        | `Backward ->
            s.buffer |> Text_buffer.delete_line
            |> apply_n (count - 1) (fun x ->
                x |> Text_buffer.move `Up |> Text_buffer.delete_line )
      in
      { s with
        buffer= new_buffer
      ; undo_stack= s.buffer :: s.undo_stack
      ; redo_stack= [] }
  | Operation {op= Change; target= Move {move= Line dir; count= c1}; count= c2}
    ->
      let count = (c1 * c2) + 1 in
      let new_buffer =
        match dir with
        | `Forward ->
            s.buffer
            |> apply_n (count - 1) Text_buffer.delete_line
            |> Text_buffer.reset_line
        | `Backward ->
            s.buffer
            |> apply_n (count - 1) (fun x ->
                x |> Text_buffer.delete_line |> Text_buffer.move `Up )
            |> Text_buffer.reset_line
      in
      { s with
        mode= Insert
      ; buffer= new_buffer
      ; undo_stack= s.buffer :: s.undo_stack
      ; redo_stack= [] }
  | Operation
      {count; op= Delete; target= Move {move= Word {style; part; dir}; _}}
  | Navigation {count; move= Word {style; part; dir}} ->
      (* selecting correct functions *)
      let parse_function =
        match part with
        | `Start ->
            Text_buffer.parse_till_start
        | `End ->
            Text_buffer.parse_till_end
      in
      let classify_func = Text_buffer.get_class style in
      let peek_func, rev_peek, step_func, rev_step =
        match dir with
        | `Forward ->
            ( Text_buffer.peek_forward
            , Text_buffer.peek_backward
            , Text_buffer.step_forward
            , Text_buffer.step_backward )
        | `Backward ->
            ( Text_buffer.peek_backward
            , Text_buffer.peek_forward
            , Text_buffer.step_backward
            , Text_buffer.step_forward )
      in
      let buffer = s.buffer in
      (* because in normal mode cursor i square, we need to have special
       * case for such cases:
       * End Forward    ->  wor[d] nex t 
       *                    wor d  nex[t]
       * Start Backward ->  p rev [w]ord 
       *                   [p]rev  w ord
       * this is because technically end of word is at word| but since we
       * have square cursor, we have to go back to wor|d = wor[d]
       * 
       * if there is any way to go around it, I couldn't find it ;c
       *)
      let buffer =
        match (part, dir) with
        | `End, `Forward | `Start, `Backward -> (
          match step_func buffer with
          | Some (({current_line; _} as new_buffer), _) -> (
            match (peek_func current_line, rev_peek current_line) with
            | Some left, Some right
              when classify_func left != classify_func right ->
                new_buffer
            | _ ->
                buffer )
          | None ->
              buffer )
        | _ ->
            buffer
      in
      let buffer, _ = parse_function classify_func peek_func step_func buffer in
      (* as explained above, sometimes we have move cursor so that the square
       * is focused at the relevant letter
       *)
      let buffer =
        match (dir, part) with
        | `Forward, `End | `Backward, `Start -> (
          match rev_step buffer with
          | Some (new_buffer, _) ->
              new_buffer
          | None ->
              buffer )
        | _ ->
            buffer
      in
      {s with buffer}
  | Navigation {count; move} ->
      let move_cmd =
        match move with
        | Move.Char `Forward ->
            Text_buffer.move `Right
        | Move.Char `Backward ->
            Text_buffer.move `Left
        | Move.Line `Forward ->
            Text_buffer.move `Down
        | Move.Line `Backward ->
            Text_buffer.move `Up
      in
      {s with buffer= apply_n count move_cmd s.buffer}
  | _ ->
      s

let rec handle_input (s : t) (k : Notty.Unescape.key) : t =
  match s.mode with
  | Normal -> (
    match k with
    (* Escape should always reset parser *)
    | `Escape, _ ->
        {s with parser= Parser.init}
    (* HACK: pass ctrl+r as a char *)
    | `ASCII 'R', m when List.mem `Ctrl m ->
        handle_input s (`ASCII '\018', m)
    | `ASCII c, _ -> (
        let parser = Parser.step s.parser c in
        match parser with
        | Parser.Continue _ ->
            {s with parser}
        | Parser.Finished c ->
            let s = execute_command c s in
            {s with parser= Parser.init; last_cmd= Some c}
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
