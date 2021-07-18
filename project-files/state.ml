open Board
open Command
open Player

type t = {
  players : Player.t * Player.t;
  score : int * int;
  board : Board.t;
}

type state_result =
  | Valid of Board.t * (int * int) list
  | Invalid

type move = int list

let go brd plyr mv =
  assert (List.length mv = 4);
  let move =
    match mv with
    | [ a; b; c; d ] -> ((a, b), (c, d))
    | _ -> failwith "Invalid move Error"
  in
  let check_move = Board.get_branch move brd in
  match check_move with
  | Red | Blue -> Invalid
  | Blank ->
      let new_brd = update_board move (Player.color plyr) brd in
      Valid (new_brd, last_filled new_brd)
