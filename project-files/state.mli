(** State of game which stores players, score, and board information. *)
open Board

open Command
open Player

(** Abstract type of state. *)
type t

(** Result of current state. If result is valid, [state_result]
    represents current board and filled boxes as a result of the move.
    Otherwise returns invalid if move is already made. *)
type state_result =
  | Valid of Board.t * (int * int) list
  | Invalid

(** move represents a valid move on the board. Precondition: Move is
    valid. *)
type move = int list

(** [go board player move] is the board that is created after [player]
    makes [move] on [board]. Returns Invalid if the move has already
    been made on the board. *)
val go : Board.t -> Player.t -> move -> state_result
