let ( <|> ) a b = match a with Some _ -> a | None -> b

let digit_of_char c = int_of_string (String.make 1 c)

module MotionParser = struct
  type state =
    | Start
    | Counting of int
    | WithScope of {count: int; scope: Motion.scope}
  [@@deriving show]

  type t = Continue of state | Finished of Motion.t | Fail [@@deriving show]

  let initial : t = Continue Start

  let is_start = function Continue Start -> true | _ -> false

  let step (mp : t) (c : char) : t =
    match mp with
    | Continue s -> (
      match s with
      | Start -> (
        match c with
        | '1' .. '9' ->
            Continue (Counting (digit_of_char c))
        | _ ->
            Move.of_char c
            |> Option.map (fun move -> Finished (Move {count= 1; move}))
            <|> ( Motion.scope_of_char c
                |> Option.map (fun scope ->
                    Continue (WithScope {count= 1; scope}) ) )
            |> Option.fold ~none:Fail ~some:(fun x -> x) )
      | Counting n -> (
        match c with
        | '0' .. '9' ->
            Continue (Counting ((n * 10) + digit_of_char c))
        | _ ->
            Move.of_char c
            |> Option.map (fun move -> Finished (Move {count= n; move}))
            <|> ( Motion.scope_of_char c
                |> Option.map (fun scope ->
                    Continue (WithScope {count= n; scope}) ) )
            |> Option.fold ~none:Fail ~some:(fun x -> x) )
      | WithScope {count; scope} -> (
        match Motion.textobject_of_char c with
        | Some obj ->
            Finished (TextObject {count; scope; obj})
        | None ->
            Fail ) )
    (* Finished and Fail do nothing *)
    (* NOTE: maybe should fail? *)
    | Finished _ | Fail ->
        mp
end

type state =
  | Start
  | WithCount of int
  | OperatorPending of {op: Command.operator; count: int; motion: MotionParser.t}
[@@deriving show]

type t = Continue of state | Finished of Command.t | Fail [@@deriving show]

let init : t = Continue Start

let step (p : t) (c : char) : t =
  match p with
  | Continue s -> (
    match s with
    | Start -> (
      match c with
      (* NOTE: we cannot start counting with 0 and it has special meaning *)
      | '1' .. '9' ->
          Continue (WithCount (digit_of_char c))
      | _ ->
          Move.of_char c
          |> Option.map (fun move -> Finished (Navigation {count= 1; move}))
          <|> ( Command.immediate_of_char c
              |> Option.map (fun action -> Finished (Action {count= 1; action}))
              )
          <|> ( Command.switch_of_char c
              |> Option.map (fun s -> Finished (Switch s)) )
          <|> ( Command.operation_of_char c
              |> Option.map (fun op ->
                  Continue
                    (OperatorPending {count= 1; op; motion= MotionParser.initial}
                    ) ) )
          |> Option.fold ~none:Fail ~some:(fun x -> x) )
    | WithCount n -> (
      match c with
      | '0' .. '9' ->
          Continue (WithCount ((n * 10) + digit_of_char c))
      | _ ->
          Move.of_char c
          |> Option.map (fun move -> Finished (Navigation {count= n; move}))
          <|> ( Command.immediate_of_char c
              |> Option.map (fun action -> Finished (Action {count= n; action}))
              )
          <|> ( Command.operation_of_char c
              |> Option.map (fun op ->
                  Continue
                    (OperatorPending {count= n; op; motion= MotionParser.initial}
                    ) ) )
          |> Option.fold ~none:Fail ~some:(fun x -> x) )
    | OperatorPending {op; count; motion} -> (
        (* NOTE: cc/yy and similar are syntactic sugar. We cannot implement them
         * inside of MotionParser, because MP would have to be aware of the
         * current operator which is not in it's scope.
         *)
        let double_tap =
          MotionParser.is_start motion
          &&
          match (op, c) with
          | Delete, 'd' ->
              true
          | Change, 'c' ->
              true
          | Yank, 'y' ->
              true
          | _ ->
              false
        in
        if double_tap then Finished (Operation {count; op; target= Line})
        else
          let motion = MotionParser.step motion c in
          match motion with
          | MotionParser.Finished target ->
              Finished (Operation {count; op; target})
          | MotionParser.Continue _ ->
              Continue (OperatorPending {op; count; motion})
          | MotionParser.Fail ->
              Fail ) )
  (* Finished and Fail do nothing *)
  (* NOTE: maybe should fail? *)
  | Finished _ | Fail ->
      p
