(** Player information with name and color *)
open Board

(** Abstract type of a player. *)
type t

(** [name player] is the name associated with player. *)
val name : t -> string

(** [color player] is a Board.color of the color associated with player. *)
val color : t -> Board.color

(** [color_string player] is a string of the color associated with
    player. *)
val color_string : t -> string

(** [create_player name color] creates a new player named [name] and
    colored [color]*)
val create_player : string -> string -> t
