type 'a t = { left : 'a list; right : 'a list }

let empty = { left = []; right = [] }
let of_list l = { left = []; right = l }
let to_list z = List.rev_append z.left z.right

let is_empty { left; right } =
  match (left, right) with [], [] -> true | _ -> false

let is_start z = match z.left with [] -> true | _ -> false
let position z = List.length z.left

let move_left z =
  match z.left with [] -> z | x :: xs -> { left = xs; right = x :: z.right }

let move_right z =
  match z.right with [] -> z | x :: xs -> { left = xs; right = x :: z.right }

let insert x z = { z with left = x :: z.left }
let prepend_left items z = { z with left = List.rev_append items z.left }

let delete_left z =
  match z.left with [] -> z | _ :: xs -> { z with left = xs }
