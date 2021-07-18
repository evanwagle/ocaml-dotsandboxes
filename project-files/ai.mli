(** Easy, Medium, and Hard AI *)
open Board

(** [empty_sides row col board] returns a string list of empty sides
    based on the row and col position. *)
val empty_sides : int -> int -> Board.t -> string list

(** [easy board] is a move made by an easy level bot. Easy AI randomly
    picks available moves and returns the move in the format "r1 c2 r2
    c2". *)
val easy : Board.t -> string

(** [medium board] is a move made by a medium level bot. Medium AI
    prioritizes types of moves in this order: Completing boxes, filling
    in edges, random position. Each move is in the format "r1 c2 r2 c2" *)
val medium : Board.t -> string

(** [hard board] is a move made by a hard level bot. Hard AI prioritizes
    types of moves in this order: Completing boxes, filling in edges,
    random position. When making these moves, Hard AI tries to avoid
    moves which will create an opportunity for the other player to score
    a box. Each move is in the format "r1 c2 r2 c2" *)
val hard : Board.t -> string
