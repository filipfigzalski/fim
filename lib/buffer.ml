open Types

let empty_buffer =
  {
    lines_above = [];
    current_line = { left = []; right = [] };
    lines_below = [];
  }
