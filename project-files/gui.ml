open Command
open Board
open State
open Player
open Graphics
open Ai

let player1 = create_player "Player 1" "Red"

let player2 = create_player "Player 2" "Blue"

let bot = create_player "Bot" "Blue"

let bot1 = create_player "Bot 1" "Red"

let bot2 = create_player "Bot 2" "Blue"

let line_color_red = [ 238; 128; 128 ]

let line_color_blue = [ 72; 178; 231 ]

let box_color_red = [ 240; 89; 70 ]

let box_color_blue = [ 24; 126; 223 ]

let instructions =
  [
    "To make a move, type in this format: 'r1 c2 r2 c2'";
    "The indices are zero indexed- the top left corner represents '0 0'";
    "For example, the move from the top left corner to the dot to the \
     right of it would be encoded as:";
    "'0 0 0 1', since you are moving from the 0th row and 0th column \
     to the 0th row and the 1st column.";
  ]

let draw_grid location m n =
  match location with
  | x, y ->
      (* Drawing points *)
      for i = 0 to n do
        for j = 0 to m do
          set_color (rgb 97 97 97);
          fill_circle (x + (i * 100)) (y - (j * 100)) 15
        done
      done

let draw_row_labels rows =
  for i = 0 to rows do
    set_color black;
    moveto 92 (793 - (i * 100));
    draw_string (string_of_int i)
  done

let draw_col_labels cols =
  for i = 0 to cols do
    set_color black;
    moveto (146 + (i * 100)) 850;
    draw_string (string_of_int i)
  done

let draw_move coordinates color =
  match color with
  | r, g, b ->
      set_color (rgb r g b);
      set_line_width 12;
      moveto (List.nth coordinates 0) (List.nth coordinates 1);
      lineto (List.nth coordinates 2) (List.nth coordinates 3)

let draw_box len pos rgb_col =
  set_color
    (rgb (List.nth rgb_col 0) (List.nth rgb_col 1) (List.nth rgb_col 2));
  fill_rect (List.nth pos 0) (List.nth pos 1) len len

let draw_boxes lst player =
  let col =
    if
      Player.name player = Player.name player1
      || Player.name player = Player.name bot1
    then box_color_red
    else box_color_blue
  in
  if List.length lst = 0 then ()
  else if List.length lst = 1 then
    (* One box filled *)
    match List.hd lst with
    | r, c -> draw_box 90 [ 155 + (100 * c); 705 - (100 * r) ] col
  else
    (* Two boxes filled with one move *)
    let r1 = fst (List.hd lst) in
    let c1 = snd (List.hd lst) in
    let r2 = fst (List.nth lst 1) in
    let c2 = snd (List.nth lst 1) in
    draw_box 90 [ 155 + (100 * c1); 705 - (100 * r1) ] col;
    draw_box 90 [ 155 + (100 * c2); 705 - (100 * r2) ] col

let draw_counter loc count =
  match loc with
  | x, y ->
      moveto x y;
      set_color white;
      fill_rect x y 20 20;
      set_color black;
      draw_string (string_of_int count)

let draw_counters board =
  moveto 50 50;
  set_color (rgb 240 89 70);
  draw_string "Red: ";
  (* Set up counter label Blue *)
  moveto 700 50;
  set_color (rgb 24 126 223);
  draw_string "Blue: ";
  let scores = Board.score board in
  match scores with
  | p1, p2 ->
      draw_counter (90, 50) p1;
      draw_counter (750, 50) p2

let rec int_list_to_string lst s =
  match lst with
  | [] -> s
  | h :: t ->
      if s <> "" then int_list_to_string t (s ^ " " ^ string_of_int h)
      else int_list_to_string t (s ^ string_of_int h)

let rec char_list_to_string lst s =
  match lst with
  | [] -> s
  | h :: t -> char_list_to_string t (s ^ Char.escaped h)

let acc = ref [||]

let display_line move color =
  match move with
  | [ r1; c1; r2; c2 ] ->
      let x1 = 150 + (100 * c1) in
      let y1 = 800 - (100 * r1) in
      let x2 = 150 + (100 * c2) in
      let y2 = 800 - (100 * r2) in
      draw_move [ x1; y1; x2; y2 ] color
  | _ -> failwith "Precondition violated"

let display_current_player player =
  moveto 275 200;
  set_color white;
  fill_rect 275 200 100 20;
  set_color black;
  draw_string (Player.name player ^ "'s move")

(* - User input as char - Stored as char array - Press enter - Convert
   char array into a string (command_issued) - Goes into
   display_valid_move (parse) - display_valid_move is unit displaying on
   board - Then call player input (mutually recursive) *)
let rec display_valid_move s board player mode =
  let parsed = parse s board in
  moveto 275 250;
  match parsed with
  | Legal move -> (
      match State.go board player move with
      | Valid (bo, li) ->
          let color =
            if Player.name player = Player.name player1 then
              (* (238, 128, 128) *)
              ( List.nth line_color_red 0,
                List.nth line_color_red 1,
                List.nth line_color_red 2 )
            else
              ( List.nth line_color_blue 0,
                List.nth line_color_blue 1,
                List.nth line_color_blue 2 )
          in
          display_line move color;
          draw_boxes li player;
          moveto 275 130;
          draw_string ("Legal move: " ^ int_list_to_string move "");
          if mode = "Mult" then
            if Player.name player = Player.name player1 then
              if List.length li > 0 then (bo, player1) else (bo, player2)
            else if List.length li > 0 then (bo, player2)
            else (bo, player1)
          else if List.length li > 0 then (bo, player1)
          else ai_move board player mode
      | Invalid ->
          display_line [ 0; 0; 0; 0 ] (120, 120, 120);
          moveto 275 130;
          draw_string "This move has already been done!";
          (board, player))
  | Illegal ->
      display_line [ 0; 0; 0; 0 ] (120, 120, 120);
      moveto 275 130;
      draw_string "This is an illegal move!";
      (board, player)

and ai_move board player mode =
  Unix.sleep 1;
  let move =
    if mode = "Easy" then Ai.easy board
    else if mode = "Medium" then Ai.medium board
    else Ai.hard board
  in
  let ai_parsed = parse move board in
  match ai_parsed with
  | Legal list_move -> (
      match State.go board bot list_move with
      | Valid (bo, li) ->
          display_line list_move
            ( List.nth line_color_blue 0,
              List.nth line_color_blue 1,
              List.nth line_color_blue 2 );
          draw_boxes li bot;
          moveto 275 225;
          set_color white;
          fill_rect 275 225 120 20;
          set_color (rgb 0 0 0);
          draw_string ("Bot move: " ^ int_list_to_string list_move "");
          if List.length li = 0 || Board.end_game bo then (bo, player1)
          else ai_move bo bot mode
      | Invalid ->
          failwith "impossible, bot will always make a valid move")
  | Illegal -> failwith "bot will always make a legal move"

let rec player_input () board player mode =
  let event = wait_next_event [ Key_pressed ] in
  match event.key with
  | 'q' -> close_graph ()
  | '\r' -> command_issued acc board player mode
  | key -> char_input acc key board player mode

and command_issued acc board player mode =
  (* //convert array into a string pass that string into
     display_valid_move make the array empty player_input()*)
  let a = Array.to_list !acc in
  let str = char_list_to_string a "" in
  set_color white;
  fill_rect 250 100 300 75;
  let new_setup = display_valid_move str board player mode in
  acc := [||];
  let brd = fst new_setup in
  let plyr = snd new_setup in
  draw_counters brd;
  display_current_player plyr;
  if Board.end_game brd then end_game brd plyr mode
  else player_input () brd plyr mode

and end_game brd plyr mode =
  set_color white;
  fill_rect 0 0 800 1000;
  set_color black;
  moveto 300 550;
  match Board.score brd with
  | p1score, p2score ->
      let p1name =
        if mode = "Simulation" then Player.name bot1
        else Player.name player1
      in
      let p2name =
        if mode = "Mult" then Player.name player2
        else if mode = "Simulation" then Player.name bot2
        else if mode = "Easy" then "The Easy Bot"
        else if mode = "Medium" then "The Medium Bot"
        else "The Hard Bot"
      in
      if p1score > p2score then
        draw_string (p1name ^ " wins, the game is over! GGWP")
      else draw_string (p2name ^ " wins, the game is over! GGWP");
      moveto 300 500;
      set_color red;
      draw_string (p1name ^ " score: " ^ string_of_int p1score);
      moveto 300 450;
      set_color blue;
      draw_string (p2name ^ " score: " ^ string_of_int p2score);
      moveto 200 400;
      draw_string
        "Press Q to quit out of the game! Thanks for playing. Try all \
         of the different bot difficulties";
      player_input () brd plyr mode

and char_input acc key board player mode =
  acc := Array.append !acc [| key |];
  moveto 275 150;
  for i = 0 to Array.length !acc - 1 do
    draw_char (Array.get !acc i)
  done;
  player_input () board player mode

and loop_simulation board bot_diff bot2_diff current_bot =
  if Board.end_game board then end_game board current_bot "Simulation"
  else simulation_move board bot_diff bot2_diff current_bot

and proper_move board bot_diff bot2_diff current_bot =
  let current_diff =
    if Player.name current_bot = Player.name bot1 then bot_diff
    else bot2_diff
  in
  let move =
    if current_diff = "Easy" then Ai.easy board
    else if current_diff = "Medium" then Ai.medium board
    else Ai.hard board
  in
  move

and simulation_move board bot_diff bot2_diff current_bot =
  Unix.sleep 1;
  if Board.end_game board then ()
  else
    let bot_move = proper_move board bot_diff bot2_diff current_bot in
    match parse bot_move board with
    | Legal list_bot_move -> (
        match State.go board current_bot list_bot_move with
        | Valid (bo, li) ->
            let color =
              if Player.name current_bot = Player.name bot1 then
                (* (238, 128, 128) *)
                ( List.nth line_color_red 0,
                  List.nth line_color_red 1,
                  List.nth line_color_red 2 )
              else
                ( List.nth line_color_blue 0,
                  List.nth line_color_blue 1,
                  List.nth line_color_blue 2 )
            in
            display_line list_bot_move color;
            draw_boxes li current_bot;
            draw_counters bo;
            display_current_player current_bot;
            if List.length li = 0 || Board.end_game bo then
              if Player.name current_bot = Player.name bot1 then
                loop_simulation bo bot_diff bot2_diff bot2
              else loop_simulation bo bot_diff bot2_diff bot1
            else simulation_move bo bot_diff bot2_diff current_bot
        | Invalid ->
            failwith "impossible, bot will always make a valid move ")
    | Illegal -> failwith "bot will always make a legal move"

let board_dimensions = (5, 5)

let window_dimensions = (800, 1000)

let counter_dimensions = (250, 100, 300, 75)

let init_graph pos = open_graph ""

let draw_instructions pos =
  for i = 0 to List.length instructions - 1 do
    match pos with
    | x, y ->
        moveto x (y - (i * 15));
        draw_string (List.nth instructions i)
  done

let draw_board brd_dim win_dim count_dim mode =
  let default_board = make_board (fst brd_dim, snd brd_dim) in
  init_graph (0, 0);
  resize_window (fst win_dim) (snd win_dim);
  set_window_title "Dots and Boxes";
  (* Set up Game Title *)
  moveto 350 900;
  set_color black;
  draw_string "Dots and Boxes!";
  draw_grid (150, 800) (fst brd_dim) (snd brd_dim);
  set_color black;
  set_line_width 3;
  draw_instructions (130, 980);
  match count_dim with
  | x, y, w, h ->
      draw_rect x y w h;
      draw_counters default_board;
      draw_row_labels (fst brd_dim);
      draw_col_labels (snd brd_dim);
      display_current_player player1;
      if mode = "Simulation" then
        loop_simulation default_board "Medium" "Hard" bot1
      else player_input () default_board player1 "Medium"

let open_board =
  draw_board board_dimensions window_dimensions counter_dimensions
    "Simulation"
