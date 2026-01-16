type line_zipper = { left : char list; right : char list }

type text_buffer = {
  lines_above : string list;
  current_line : line_zipper;
  lines_below : string list;
}

type mode = Normal | Insert | Command
type editor_state = { mode : mode; buffer : text_buffer; curswant : int }
