open Notty
open Notty_unix
open Fim.Types
open Fim.Buffer

let render state = I.string A.empty "Test renderowania"

let rec loop term state =
  Term.image term (render state);
  match Term.event term with `Key (`Enter, _) -> () | _ -> loop term state

let () =
  let term = Term.create () in
  let initial_state = { mode = Normal; buffer = empty_buffer; curswant = 0 } in
  loop term initial_state
