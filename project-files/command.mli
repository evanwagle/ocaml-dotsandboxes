(** Parsing of player moves. *)
open Board

(** The type of parses. A parse either produces a Legal list of integers
    which represents the legal move, i.e. "1 1 1 2" would translate to
    Legal [1; 1; 1; 2]. If the move is not legal, then it is illegal.*)
type result =
  | Legal of int list
  | Illegal

(** A move has the form r1 c1 r2 c2. This means the player is drawing a
    line from point (r1,c1) to point (r2,c2). [parse str] takes a
    player's string input and decides if it is legal or not. If it is
    legal, it returns a type Legal of int list representing their move.
    For example, parse "2 3 3 3" would return Legal [2; 3; 3; 3]. But
    parse "banana" would return Illegal.*)
val parse : string -> Board.t -> result
