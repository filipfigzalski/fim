type direction = [`Left | `Right | `Up | `Down]

type rel_direction = Before | After | Below | Above

type motion =
  | MMove of direction
  | MWord of
      { style: Text_buffer.word_style
      ; direction: Text_buffer.word_direction
      ; part: Text_buffer.word_part }

type action =
  | Act_Digit of int
  | Act_Motion of motion
  | Act_Insert of rel_direction
  | Act_Esc

let normal_bindings =
  [ (* Motions *)
    ('h', Act_Motion (MMove `Left))
  ; ('j', Act_Motion (MMove `Down))
  ; ('k', Act_Motion (MMove `Up))
  ; ('l', Act_Motion (MMove `Right))
  ; ('w', Act_Motion (MWord {direction= Forward; style= Word; part= Start}))
  ; ('W', Act_Motion (MWord {direction= Forward; style= WORD; part= Start}))
  ; ('e', Act_Motion (MWord {direction= Forward; style= Word; part= End}))
  ; ('E', Act_Motion (MWord {direction= Forward; style= WORD; part= End}))
  ; ('b', Act_Motion (MWord {direction= Backward; style= Word; part= End}))
  ; ('B', Act_Motion (MWord {direction= Backward; style= WORD; part= End}))
  ; (* Mode switching *)
    ('i', Act_Insert Before)
  ; ('a', Act_Insert After)
  ; ('o', Act_Insert Below)
  ; ('O', Act_Insert Above) ]

let get_action key =
  match List.assoc_opt key normal_bindings with
  | Some action ->
      action
  | None -> (
    match key with
    (* HACK: we use `escaped` to convert to string *)
    | '0' .. '9' ->
        Act_Digit (int_of_string (Char.escaped key))
    | _ ->
        Act_Esc )

type normal_ctx = {motion_count: int option}

type _ mode = Normal : normal_ctx -> [`Normal] mode | Insert : [`Insert] mode

type 'm state = {mode: 'm mode; buffer: Text_buffer.t}

type any_state = Any : 'm state -> any_state

let initial = {mode= Insert; buffer= Text_buffer.empty_buffer}

let empty_normal_ctx = {motion_count= None}

let initial = {mode= Normal empty_normal_ctx; buffer= Text_buffer.empty_buffer}

let add_digit d v = (v * 10) + d

let normal_transition (state : [`Normal] state) (action : action) : any_state =
  let (Normal ctx) = state.mode in
  match action with
  | Act_Insert Before ->
      Any {state with mode= Insert}
  | Act_Insert After ->
      Any {buffer= Text_buffer.move `Right state.buffer; mode= Insert}
  | Act_Insert Below ->
      let buffer = Text_buffer.move `End state.buffer in
      let buffer = Text_buffer.newline buffer in
      Any {buffer; mode= Insert}
  | Act_Insert Above ->
      let buffer = Text_buffer.move `Start state.buffer in
      let buffer = Text_buffer.newline buffer in
      let buffer = Text_buffer.move `Up buffer in
      Any {buffer; mode= Insert}
  | Act_Esc ->
      Any {state with mode= Normal empty_normal_ctx}
  | Act_Digit d ->
      let motion_count =
        ctx.motion_count |> Option.value ~default:0 |> add_digit d
        |> Option.some
      in
      Any {state with mode= Normal {motion_count}}
  | Act_Motion m -> (
    match m with
    | MMove d ->
        Any {state with buffer= Text_buffer.move d state.buffer}
    | MWord {style; direction; part} ->
        Any
          { state with
            buffer= Text_buffer.move_word style direction part state.buffer } )

let insert_transition (state : [`Insert] state) input : any_state =
  match input with
  (* we need to move left to match with original vim behaviour *)
  | `Key (`Escape, _) ->
      Any
        { buffer= Text_buffer.move `Left state.buffer
        ; mode= Normal empty_normal_ctx }
  | `Key (`Backspace, _) ->
      Any {state with buffer= Text_buffer.backspace state.buffer}
  | `Key (`Enter, _) ->
      Any {state with buffer= Text_buffer.newline state.buffer}
  | `Key (`ASCII c, _) ->
      Any
        { state with
          buffer= Text_buffer.insert_uchar (Uchar.of_char c) state.buffer }
  | `Key (`Uchar u, _) ->
      Any {state with buffer= Text_buffer.insert_uchar u state.buffer}
  | _ ->
      Any state

let handle_input (Any state) input : any_state =
  match state.mode with
  | Insert ->
      insert_transition state input
  | Normal _ -> (
    match input with
    | `Key (`ASCII c, _) ->
        let action = get_action c in
        normal_transition state action
    | _ ->
        Any state )
