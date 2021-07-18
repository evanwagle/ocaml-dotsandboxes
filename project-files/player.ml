open Board

type t = string * Board.color

let name player = fst player

let color player = snd player

let color_string player =
  match snd player with
  | Red -> "Red"
  | Blue -> "Blue"
  | Blank -> "Blank"

let create_player name color =
  assert (color = "Red" || color = "Blue");
  let clr =
    match color with
    | "Red" -> Red
    | "Blue" -> Blue
    | _ -> failwith "Error in Assertion"
  in
  (name, clr)
