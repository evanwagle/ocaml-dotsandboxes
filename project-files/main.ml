open Board
open Command
open State
open Player
open Ai

let init_board = Board.make_board (5, 5)

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

let bot = create_player "Bot" "Blue"

let bot1 = create_player "Bot 1" "Red"

let bot2 = create_player "Bot 2" "Blue"

(* The number of moves player 1 has made *)
let p1moves = ref 0

(* The number of moves player 2 has made *)
let p2moves = ref 0

(* The number of moves the bot has made *)
let botmoves = ref 0

let inc_move player_moves = incr player_moves

(* When the game ends, displays who won and each players respective
   score *)
let display_endgame board mode =
  if mode = "Simulation" then
    let p1name = Player.name bot1 in
    let p2name = Player.name bot2 in
    match Board.score board with
    | p1score, p2score ->
        if p1score > p2score then
          print_string
            ("\n" ^ "------------------------------\n" ^ p1name
           ^ " wins, the simulation is over! Bots reign supreme.\n"
           ^ p1name ^ " score: " ^ string_of_int p1score ^ "\n" ^ p2name
           ^ " score: " ^ string_of_int p2score ^ "\n"
           ^ "------------------------------\n")
        else
          print_string
            ("\n" ^ "------------------------------\n" ^ p2name
           ^ " wins, the simulation is over! Bots reign supreme.\n"
           ^ p1name ^ " score: " ^ string_of_int p1score ^ "\n" ^ p2name
           ^ " score: " ^ string_of_int p2score ^ "\n"
           ^ "------------------------------\n")
  else
    let p2name =
      if mode = "Mult" then Player.name player2
      else if mode = "Easy" then "Easy Bot"
      else if mode = "Medium" then "Medium Bot"
      else "Hard Bot"
    in
    match Board.score board with
    | p1score, p2score ->
        if p1score > p2score then
          print_string
            ("\n" ^ "------------------------------\n"
           ^ Player.name player1 ^ " wins, the game is over! GGWP\n"
           ^ Player.name player1 ^ " score: " ^ string_of_int p1score
           ^ "\n" ^ p2name ^ " score: " ^ string_of_int p2score ^ "\n"
           ^ "------------------------------\n")
        else
          print_string
            ("\n" ^ "------------------------------\n" ^ p2name
           ^ " wins, the game is over! GGWP\n" ^ Player.name player1
           ^ " score: " ^ string_of_int p1score ^ "\n" ^ p2name
           ^ " score: " ^ string_of_int p2score ^ "\n"
           ^ "------------------------------\n")

let rec loop_mode mode =
  ANSITerminal.print_string [ ANSITerminal.red ] mode;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\
    \ Type the mode you want to play: Easy | Medium | Hard | \
     Multiplayer | Simulation\n";
  match read_line () with
  | exception End_of_file -> ()
  | player_input ->
      let player_input = String.trim player_input in
      if player_input = "Multiplayer" then
        loop_game init_board player1 "" "Mult"
      else if player_input = "Easy" then
        loop_game init_board player1 "" "Easy"
      else if player_input = "Medium" then
        loop_game init_board player1 "" "Medium"
      else if player_input = "Hard" then
        loop_game init_board player1 "" "Hard"
      else if player_input = "Simulation" then bot_difficulty ""
      else loop_mode "\n Error: Not a valid mode!\n"

and bot_difficulty diff =
  ANSITerminal.print_string [ ANSITerminal.red ] diff;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n Type the difficulty of bot 1: Easy | Medium | Hard \n";
  match read_line () with
  | exception End_of_file -> ()
  | player_input ->
      let player_input = String.trim player_input in
      if player_input = "Easy" then bot2_difficulty "" "Easy"
      else if player_input = "Medium" then bot2_difficulty "" "Medium"
      else if player_input = "Hard" then bot2_difficulty "" "Hard"
      else bot_difficulty "\n Error: Not a valid difficulty!\n"

and bot2_difficulty diff bot_diff =
  ANSITerminal.print_string [ ANSITerminal.red ] diff;
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n Type the difficulty of bot 2: Easy | Medium | Hard \n";
  match read_line () with
  | exception End_of_file -> ()
  | player_input ->
      let player_input = String.trim player_input in
      if player_input = "Easy" then
        loop_simulation init_board bot_diff "Easy" bot1
      else if player_input = "Medium" then
        loop_simulation init_board bot_diff "Medium" bot1
      else if player_input = "Hard" then
        loop_simulation init_board bot_diff "Hard" bot1
      else
        bot2_difficulty "\n Error: Not a valid difficulty!\n" bot_diff

and loop_simulation board bot_diff bot2_diff current_bot =
  if Board.end_game board then display_endgame board "Simulation"
  else
    let botname = Player.name current_bot in
    print_endline (botname ^ "'s" ^ " turn.\n");
    print_string "> ";
    simulation_move board bot_diff bot2_diff current_bot

and proper_move board bot_diff bot2_diff current_bot =
  let current_diff =
    if Player.name current_bot = Player.name bot1 then bot_diff
    else bot2_diff
  in
  ANSITerminal.print_string [ ANSITerminal.red ] current_diff;
  let move =
    if current_diff = "Easy" then Ai.easy board
    else if current_diff = "Medium" then Ai.medium board
    else Ai.hard board
  in
  move

and simulation_move board bot_diff bot2_diff current_bot =
  (* Unix.sleep 1; *)
  if Board.end_game board then display_endgame board "Simulation"
  else
    let bot_move = proper_move board bot_diff bot2_diff current_bot in
    ANSITerminal.print_string [ ANSITerminal.red ]
      ("\nBot move: " ^ bot_move ^ "\n");
    match Command.parse bot_move board with
    | Legal t -> (
        match State.go board current_bot t with
        | Valid (bo, li) ->
            (* If player did not get a box, switches to other player's
               turn *)
            if List.length li = 0 then
              if Player.name current_bot = Player.name bot1 then
                loop_simulation bo bot_diff bot2_diff bot2
              else loop_simulation bo bot_diff bot2_diff bot1
            else if Board.end_game bo then
              display_endgame bo "Simulation"
            else simulation_move bo bot_diff bot2_diff current_bot
        | Invalid ->
            failwith "impossible, bot will always make a valid move")
    | Illegal ->
        failwith "impossible, bot will always make a legal move"

and loop_game board player input mode =
  if Board.end_game board then display_endgame board mode
  else
    let playername = Player.name player in
    print_endline (playername ^ "'s" ^ " turn.\n");
    print_string "> ";
    match read_line () with
    | exception End_of_file -> ()
    | player_move -> parse_input board player player_move mode

(* Based on the players input, either quits out of the game , shows the
   player stats, shows the board, shows the score, or makes a move.*)
and parse_input board player player_move mode =
  if String.trim player_move = "quit" then exit 0;
  if String.trim player_move = "stats" then
    player_stats board player mode;
  if String.trim player_move = "board" then
    print_board board player mode;
  if String.trim player_move = "score" then score board player mode
  else if mode = "Easy" || mode = "Medium" || mode = "Hard" then
    parse_ai_input board player player_move mode
  else parse_mult_input board player player_move

(* Check the level of the bot, get the move from the bot, display it to
   the user, and make a new board based on the move.*)
and parse_ai_input board player move level =
  match Command.parse move board with
  | Legal t -> (
      match State.go board player t with
      | Valid (bo, li) ->
          (* If player did not get a box, switches to other player's
             turn *)
          inc_move p1moves;
          if List.length li = 0 then bot_move board level
          else loop_game bo player "" level
      | Invalid ->
          print_string "Position already occupied, try again. \n";
          loop_game board player move level)
  | Illegal ->
      print_string
        ("\n" ^ Player.name player
       ^ ", your move was invalid. Try again!\n");
      loop_game board player move level

and bot_move board level =
  (* Unix.sleep 1; *)
  let move =
    if level = "Easy" then Ai.easy board
    else if level = "Medium" then Ai.medium board
    else Ai.hard board
  in
  ANSITerminal.print_string [ ANSITerminal.red ]
    ("\nBot move: " ^ move ^ "\n");
  match Command.parse move board with
  | Legal t -> (
      match State.go board bot t with
      | Valid (bo, li) ->
          (* If player did not get a box, switches to other player's
             turn *)
          inc_move botmoves;
          if List.length li = 0 then loop_game bo player1 "" level
          else if Board.end_game bo then display_endgame bo level
          else bot_move bo level
      | Invalid ->
          failwith "impossible, bot will always make a valid move")
  | Illegal -> failwith "impossible, bot will always make a legal move"

and parse_mult_input board player move =
  match Command.parse move board with
  | Legal t -> (
      match State.go board player t with
      | Valid (bo, li) ->
          (* If player did not get a box, switches to other player's
             turn *)
          if Player.name player = Player.name player1 then
            inc_move p1moves
          else inc_move p2moves;
          if List.length li = 0 then
            if Player.name player = Player.name player1 then
              loop_game bo player2 "" "Mult"
            else loop_game bo player1 "" "Mult"
          else
            (*(if Player.name = Player.name player1 then inc_move
              p1moves else inc_move p2moves) *)
            loop_game bo player "" "Mult"
      | Invalid ->
          print_string "Position already occupied, try again. \n";
          loop_game board player move "Mult")
  | Illegal ->
      print_string
        ("\n" ^ Player.name player
       ^ ", your move was invalid. Try again!\n");
      loop_game board player move "Mult"

and score board player mode =
  print_string
    ("\n" ^ "-------------------------\n" ^ Player.name player1
   ^ " score: "
    ^ string_of_int (fst (Board.score board))
    ^ "\n" ^ Player.name player2 ^ " score: "
    ^ string_of_int (snd (Board.score board))
    ^ "\n-------------------------" ^ "\n");
  loop_game board player "" mode

and player_stats board player mode =
  print_string
    ("\n" ^ "-------------------------\n" ^ Player.name player1
   ^ " moves: " ^ string_of_int !p1moves ^ "\n");
  if mode = "Mult" then
    print_string
      (Player.name player2 ^ " moves: " ^ string_of_int !p2moves
     ^ "\n-------------------------" ^ "\n")
  else
    print_string
      (Player.name bot ^ " moves: " ^ string_of_int !botmoves
     ^ "\n-------------------------" ^ "\n");
  loop_game board player "" mode

(* The printing feature is mainly used for debugging, it does not give a
   very good representation of the board.*)
and print_board board player mode =
  let row1 = row_colors board 0 0 5 in
  let row2 = row_colors board 0 1 5 in
  let row3 = row_colors board 0 2 5 in
  let row4 = row_colors board 0 3 5 in
  let row5 = row_colors board 0 4 5 in
  let row6 = row_colors board 0 5 5 in
  let col1 = col_colors board 0 0 5 in
  let col2 = col_colors board 1 0 5 in
  let col3 = col_colors board 2 0 5 in
  let col4 = col_colors board 3 0 5 in
  let col5 = col_colors board 4 0 5 in
  let col6 = col_colors board 5 0 5 in
  let rows = [ row1; row2; row3; row4; row5; row6 ] in
  let cols = [ col1; col2; col3; col4; col5; col6 ] in
  print_string "rows:\n";
  print_rows rows;
  print_string "cols:\n";
  print_rows cols;
  loop_game board player "" mode

(* Row_colors returns a list with just the colors in each row. I is the
   current index it is on, and j is what it is going to*)
and row_colors board x y j =
  let c = get_branch ((x, y), (x + 1, y)) board in
  if j - x = 1 then [ string_of_color c ]
  else string_of_color c :: row_colors board (x + 1) y j

and col_colors board x y j =
  let c = get_branch ((x, y), (x, y + 1)) board in
  if j - y = 1 then [ string_of_color c ]
  else string_of_color c :: col_colors board x (y + 1) j

and string_of_color c =
  match c with Red -> "R" | Blue -> "B" | Blank -> "N"

and print_rows rows =
  let print_row r =
    List.iter print_string r;
    print_string "\n-------------------------\n"
  in
  List.iter print_row rows

let main () =
  ANSITerminal.print_string [ ANSITerminal.red ]
    "\n\n\
     Welcome to Dots and Boxes!. Type 'quit' to exit the game, type \
     'stats' to view the game statistics, and \n\
    \    'score' to view the score.\n";
  print_string "> ";
  loop_mode ""

(* Execute the game engine. *)
let () = main ()
