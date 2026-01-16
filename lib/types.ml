type line = Uchar.t Zipper.t

type text_buffer = {
  lines_above : string list;
  current_line : line;
  lines_below : string list;
}

type mode = Normal | Insert | Command
type editor_state = { mode : mode; buffer : text_buffer; curswant : int }
