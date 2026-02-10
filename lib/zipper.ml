type 'a t = {left: 'a list; right: 'a list}

let empty = {left= []; right= []}

let of_list l = {left= []; right= l}

let to_list z = List.rev_append z.left z.right

let is_empty {left; right} =
  match (left, right) with [], [] -> true | _ -> false

let is_start z = match z.left with [] -> true | _ -> false

let is_end z = match z.right with [] -> true | _ -> false

let position z = List.length z.left

let focus z = match z.right with [] -> None | c :: _ -> Some c

let focus_left z = match z.left with [] -> None | c :: _ -> Some c

let move_left z =
  match z.left with [] -> z | x :: xs -> {left= xs; right= x :: z.right}

let move_right z =
  match z.right with [] -> z | x :: xs -> {left= x :: z.left; right= xs}

let move_start z = {left= []; right= List.rev_append z.left z.right}

let move_end z = {left= List.rev_append z.right z.left; right= []}

let insert x z = {z with left= x :: z.left}

let prepend_left items z = {z with left= List.rev_append items z.left}

let split z =
  let prev = {left= z.left; right= []} in
  let next = {left= []; right= z.right} in
  (prev, next)

let delete_left z = match z.left with [] -> z | _ :: xs -> {z with left= xs}

let delete_right z = match z.right with [] -> z | _ :: xs -> {z with right= xs}
