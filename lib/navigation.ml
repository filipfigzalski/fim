let is_alphanum = function
  | 'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' | '_' | '@' ->
      true
  | _ ->
      false

type char_class = Word | Space | Other

type word_style = [`Word | `WORD]

type word_direction = [`Forward | `Backward]

type word_part = [`End | `Start]

(* classify uchar based on the word style *)
let get_class style u : char_class =
  if Uchar.is_char u then
    match (style, Uchar.to_char u) with
    | _, (' ' | '\t') ->
        Space
    | `WORD, _ ->
        Word
    | `Word, c when is_alphanum c ->
        Word
    | `Word, _ ->
        Other
  else Word

(* helper to repeat a step function while condition holds. *)
let rec skip_while p f buffer =
  if p buffer then
    match f buffer with
    | None ->
        buffer
    | Some next_buffer ->
        skip_while p f next_buffer
  else buffer

let move_word (style : word_style) (direction : word_direction)
    (part : word_part) (buffer : Text_buffer.t) : Text_buffer.t =
  let next_char (b : Text_buffer.t) =
    match direction with
    | `Forward ->
        Zipper.focus b.current_line
    | `Backward ->
        Zipper.focus_left b.current_line
  in
  (* function that takes current Text_buffer state + word_style and returns
      char_class at current position
   *)
  let current_cls b =
    next_char b |> Option.map (get_class style) |> Option.value ~default:Space
  in
  (* pick right step function *)
  let step, rev_step =
    match direction with
    | `Forward ->
        (Text_buffer.step_forward, Text_buffer.step_backward)
    | `Backward ->
        (Text_buffer.step_backward, Text_buffer.step_forward)
  in
  let buffer =
    match (direction, part) with
    | `Forward, `End ->
        buffer |> step |> Option.value ~default:buffer
    | _ ->
        buffer
  in
  let buffer =
    match part with
    | `Start ->
        (* if we start at Word/Other, we must go till the end of it *)
        let start_type = current_cls buffer in
        let buffer =
          if start_type = Space then buffer
          else skip_while (fun u -> current_cls u = start_type) step buffer
        in
        (* if we are on space we also skip it *)
        let buffer = skip_while (fun u -> current_cls u = Space) step buffer in
        buffer
    | `End ->
        let start_type = current_cls buffer in
        let buffer =
          if start_type != Space then buffer
          else skip_while (fun u -> current_cls u = Space) step buffer
        in
        let word_type = current_cls buffer in
        let buffer =
          skip_while (fun u -> current_cls u = word_type) step buffer
        in
        buffer
  in
  let buffer =
    match (direction, part) with
    | `Forward, `End ->
        buffer |> rev_step |> Option.value ~default:buffer
    | `Backward, `Start ->
        buffer |> step |> Option.value ~default:buffer
    | _ ->
        buffer
  in
  {buffer with curswant= Zipper.position buffer.current_line}
