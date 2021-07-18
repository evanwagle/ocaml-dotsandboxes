(** Visual representation of game. *)
open Player

(** open_board mode opens the graph for the user and defines the
    background color and initial dimensions. *)
val open_board : unit

(** Draws the text per red or blue player *)
val draw_board : int * int -> int * int -> int * int * int * int -> unit

(** [draw_move coordinates color] takes a list of coordinates which is
    are starting and ending points Ex: 1 3 4 2 represents (1,3) (4,2)
    and a color, and draws the move. Precondition: coordinates is a
    valid list of two coordinates, color is a RGB representation of a
    color*)
val draw_move : int list -> int * int * int -> unit

(** [draw_box len pos rgb_col] draws a box on the screen with side
    length len, positioned with its bottom left corner at pos. The box
    will be filled in with color represented by rgbcol, which is an list
    of length 3 where each index represents r, g, b in the rgb color
    model. *)
val draw_box : int -> int list -> int list -> unit

(** [draw_boxes lst player] draws the boxes on the board based on the
    player and the number of boxes the player can fill. *)
val draw_boxes : (int * int) list -> Player.t -> unit

(** [draw grid location m n ] draws a grid of circles, with the
    respective amount of m rows and n columns, starting at the top left
    of the board. *)
val draw_grid : int * int -> int -> int -> unit

(** [draw_counter loc count] Displays the score count of a player at a
    loc on the board.*)
val draw_counter : int * int -> int -> unit

(** [draw_counters board] Displays the respective color and score
    counters for both players *)
val draw_counters : Board.t -> unit

(** [display_valid_move s board player mode] displays moves based on
    what mode is passed in. If the mode is "Mult" then multiplayer mode
    starts, otherwise the mode is the chosen AI difficulty. Moves for
    each player is displayed on the board. *)
val display_valid_move :
  string -> Board.t -> Player.t -> string -> Board.t * Player.t

(** [player_input () board] waits for user input and responds depending
    on which key is pressed. Displays respective player moves and shows
    end game screen. *)
val player_input : unit -> Board.t -> Player.t -> string -> unit

(** [draw_instructions pos] takes in a position and draws the
    instructions on the board *)
val draw_instructions : int * int -> unit

(** [draw_board brd_dim win_dim count_dim mode] draws the board based on
    board dimensions, window dimensions, and count dimensions. mode
    specifies if this is a simluation or involves player input. Displays
    grid, labels, counters, players, instructions, and moves. *)
val draw_board :
  int * int -> int * int -> int * int * int * int -> string -> unit
