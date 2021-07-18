(** Test Plan: For testing, we wanted to test as many possible game
    combinations and conditions. Our goal was to have our test suite be
    exaustive of the cases a user could encounter while not being
    repetitive. To do this, we complied our test suite by looking at
    each .ml file and deciding which functions contained significant
    variables that could lead to sources of error. We also performed a
    lot of front-end user testing by running the application in the GUI
    and playing the game. This allowed us to visibly see what errors a
    player would encounter and work backwards to solve those errors in
    our code.

    Our OUnit tests were done using a combination of glass and black box
    testing. By breaking our game down into its individual features like
    the board size, level difficulty, colors, and user commands we could
    test each of these individually. Glass-box testing was used on the
    side matrix to check each side of the possible squares the user
    could create. Glass-box testing was also used on the Command module,
    since we tried every possible way of making a move. Randomized
    testing was used to test the AI. Black box ensured that the game
    ended.

    Modules tested in OUnit: Mainly command and board.

    Modules tested manually: Since we wrote code to have two bots play
    against each other, every module was "tested" manually- in the sense
    that the bots always play a complete and correct game. Since their
    moves are random, repeated simulations prove the correctness of our
    system.

    By combining random testing through AI simulation games and OUnit
    testing we are able to confirm the overall correctness of our
    system. The game has sucessfully kept track of moves, if the game
    has ended, and boxes through hundreds of AI simulations.*)

open OUnit2
open Board
open Command
open Player
open State
open Ai

let color_printer = function
  | Red -> "Red"
  | Blue -> "Blue"
  | Blank -> "Blank"

let extract_t (st : result) =
  match st with Legal t -> t | Illegal -> failwith "no info"

let extract_brd (st : state_result) =
  match st with Valid (brd, _) -> brd | Invalid -> failwith "No Board"

let default_player = create_player "Player 1" "Red"

let second_player = create_player "Player 2" "Blue"

let valid_parse_test
    (name : string)
    (move : string)
    (board : Board.t)
    (expected_output : int list) : test =
  name >:: fun _ ->
  assert_equal expected_output (extract_t (parse move board))

let fail_parse_test
    (name : string)
    (move : string)
    (board : Board.t)
    (expected_output : Command.result) : test =
  name >:: fun _ -> assert_equal expected_output (parse move board)

let string_printer str = str

let int_tuple_printer tup =
  string_of_int (fst tup) ^ ", " ^ string_of_int (snd tup)

let default_board = make_board (4, 5)

let init_board = make_board (5, 5)

let square_board = make_board (6, 6)

let one_by_one = make_board (1, 1)

let board_with_move =
  update_board ((0, 0), (1, 0)) Red (make_board (4, 5))

let board_with_box =
  update_board
    ((0, 1), (0, 0))
    Blue
    (update_board
       ((1, 1), (0, 1))
       Red
       (update_board
          ((1, 0), (1, 1))
          Blue
          (update_board ((0, 0), (1, 0)) Red (make_board (1, 1)))))

let board_with_box_player1 =
  update_board
    ((0, 1), (0, 0))
    Red
    (update_board
       ((1, 1), (0, 1))
       Blue
       (update_board
          ((1, 0), (1, 1))
          Red
          (update_board ((0, 0), (1, 0)) Blue (make_board (1, 1)))))

let big_board_with_two_boxes =
  update_board
    ((0, 2), (1, 2))
    Red
    (update_board
       ((3, 3), (3, 4))
       Blue
       (update_board
          ((3, 4), (4, 4))
          Red
          (update_board
             ((4, 4), (4, 3))
             Blue
             (update_board
                ((4, 3), (3, 3))
                Red
                (update_board
                   ((0, 1), (0, 0))
                   Blue
                   (update_board
                      ((1, 1), (0, 1))
                      Red
                      (update_board
                         ((1, 0), (1, 1))
                         Blue
                         (update_board
                            ((0, 0), (1, 0))
                            Red
                            (make_board (5, 5))))))))))

let one_box_per_player =
  update_board
    ((2, 1), (2, 0))
    Red
    (update_board
       ((2, 1), (1, 1))
       Blue
       (update_board
          ((2, 0), (1, 0))
          Red
          (update_board
             ((1, 1), (1, 0))
             Blue
             (update_board
                ((1, 1), (0, 1))
                Red
                (update_board
                   ((0, 0), (1, 0))
                   Blue
                   (update_board
                      ((0, 0), (0, 1))
                      Red
                      (make_board (3, 3))))))))

let bug_target = update_board ((3, 3), (3, 4)) Red (make_board (5, 5))

let dimension_test (name : string) (board : Board.t) expected_output :
    test =
  name >:: fun _ -> assert_equal expected_output (dimensions board)

let get_branch_test
    (name : string)
    (points : (int * int) * (int * int))
    (board : Board.t)
    expected_output : test =
  name >:: fun _ ->
  assert_equal expected_output
    (get_branch points board)
    ~printer:color_printer

let direction_test
    (name : string)
    (x1 : int)
    (x2 : int)
    (y1 : int)
    (y2 : int)
    expected_output : test =
  name >:: fun _ -> assert_equal expected_output (direction x1 x2 y1 y2)
(*~printer: *)

let branch_filled_test
    (name : string)
    (points : (int * int) * (int * int))
    (board : Board.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (branch_filled points board)
    ~printer:string_of_bool

let score_test
    (name : string)
    (board : Board.t)
    (expected_output : int * int) : test =
  name >:: fun _ ->
  assert_equal expected_output (score board) ~printer:int_tuple_printer

let player_name_test
    (name : string)
    (player : Player.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output (Player.name player)
    ~printer:string_printer

let player_color_test
    (name : string)
    (player : Player.t)
    (expected_output : string) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (Player.color_string player)
    ~printer:string_printer

let end_game_test
    (name : string)
    (board : Board.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (end_game board) ~printer:string_of_bool

let sides_matrix_test
    (name : string)
    (board : Board.t)
    (x : int)
    (y : int)
    (expected_output : int) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (sides_matrix board).(x).(y)
    ~printer:string_of_int

let player_tests =
  [
    player_name_test "Get Player 1 name" default_player "Player 1";
    player_name_test "Get Player 2 name" second_player "Player 2";
    player_color_test "Get Player 1 color" default_player "Red";
    player_color_test "Get Player 2 color" second_player "Blue";
  ]

let dimensions_tests =
  [
    dimension_test "default board" default_board (4, 5);
    dimension_test "square" square_board (6, 6);
    dimension_test "one-by-one" one_by_one (1, 1);
  ]

let board_tests =
  [
    (*Direction Tests*)
    direction_test "Right" 1 2 0 0 Right;
    direction_test "Left" 2 1 0 0 Left;
    direction_test "Down" 0 0 1 2 Down;
    direction_test "Up" 0 0 0 0 Up;
    (*Get_Branch Tests*)
    get_branch_test "default board has no branch (0,0) -> (1,0)"
      ((0, 0), (1, 0))
      default_board Blank;
    get_branch_test "board with move has a red branch (0,0) -> (1,0)"
      ((0, 0), (1, 0))
      board_with_move Red;
    get_branch_test "board with move has a red branch (1,0) -> (0,0)"
      ((1, 0), (0, 0))
      board_with_move Red;
    get_branch_test "board with box has a red branch (0,0) -> (1,0)"
      ((0, 0), (1, 0))
      board_with_box Red;
    get_branch_test "board with box has a blue branch (0,0) -> (0,1)"
      ((0, 0), (0, 1))
      board_with_box Blue;
    get_branch_test
      "bug target board should have no branch (4,3) -> (4,4)"
      ((4, 3), (4, 4))
      bug_target Blank;
    get_branch_test "bug target should have a branch (3,3) -> (3,4)"
      ((3, 3), (3, 4))
      bug_target Red;
    get_branch_test "init board should have no branch (4,4) -> (4,3)"
      ((4, 4), (4, 3))
      init_board Blank;
    get_branch_test "bbwtb has a branch (0,0) -> (1,0)"
      ((0, 0), (1, 0))
      big_board_with_two_boxes Red;
    get_branch_test "bbwtb has a branch (1,0) -> (1,1)"
      ((1, 0), (1, 1))
      big_board_with_two_boxes Blue;
    get_branch_test "bbwtb has a branch (1,1) -> (0,1)"
      ((1, 1), (0, 1))
      big_board_with_two_boxes Red;
    get_branch_test "bbwtb has a branch (0,1) -> (0,0)"
      ((0, 1), (0, 0))
      big_board_with_two_boxes Blue;
    get_branch_test "bbwtb has a branch (3,3) -> (3,4)"
      ((3, 3), (3, 4))
      big_board_with_two_boxes Blue;
    get_branch_test "bbwtb has a branch (3,4) -> (4,4)"
      ((3, 4), (4, 4))
      big_board_with_two_boxes Red;
    get_branch_test "bbwtb has a branch (4,4) -> (4,3)"
      ((4, 4), (4, 3))
      big_board_with_two_boxes Blue;
    get_branch_test "bbwtb has a branch (4,3) -> (3,3)"
      ((4, 3), (3, 3))
      big_board_with_two_boxes Red;
    get_branch_test "bbwtb has no branch (0,1) -> (0,2)"
      ((0, 1), (0, 2))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (1,1) -> (1,2)"
      ((1, 1), (1, 2))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (1,1) -> (2,1)"
      ((1, 1), (2, 1))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (1,0) -> (2,0)"
      ((2, 0), (1, 0))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (3,3) -> (2,3)"
      ((3, 3), (2, 3))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (3,3) -> (3,2)"
      ((3, 3), (3, 2))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (4,3) -> (4,2)"
      ((4, 3), (4, 2))
      big_board_with_two_boxes Blank;
    get_branch_test "bbwtb has no branch (4,3) -> (5,3)"
      ((4, 3), (5, 3))
      big_board_with_two_boxes Blank;
    (*Branch_filled*)
    branch_filled_test "default board is not filled -> false"
      ((0, 0), (1, 0))
      default_board false;
    branch_filled_test "board with move has a red branch -> true"
      ((0, 0), (1, 0))
      board_with_move true;
    branch_filled_test "board with box has a blue branch -> true"
      ((0, 0), (0, 1))
      board_with_box true;
  ]

let score_tests =
  [
    score_test "default board has score (0,0)" default_board (0, 0);
    score_test "board with box has score (0,1)" board_with_box (0, 1);
    score_test "board with two boxes has score (0,2)"
      big_board_with_two_boxes (0, 2);
    score_test "board with one box for each player has score (1,1)"
      one_box_per_player (1, 1);
    score_test "board with box Player1 has score (1,0)"
      board_with_box_player1 (1, 0);
  ]

let end_game_tests =
  [
    end_game_test "default board has not reached end game" default_board
      false;
    end_game_test "board with box has reached end game" board_with_box
      true;
    end_game_test "board with 2 boxes has reached end game"
      big_board_with_two_boxes false;
  ]

let side_matrix_tests =
  [
    sides_matrix_test "board is initialized with sides matrix vals of 0"
      default_board 0 0 0;
    sides_matrix_test
      "board_with_move should have value of 1 under the move"
      board_with_move 0 0 1;
    sides_matrix_test
      "board_with_move should have value of 0 away from the move"
      board_with_move 3 3 0;
    sides_matrix_test
      "board_with_two_boxes should have value of 1 at box (3,4)"
      big_board_with_two_boxes 3 4 1;
    sides_matrix_test
      "board_with_two_boxes should have value of 1 at box (4,3)"
      big_board_with_two_boxes 4 3 1;
    sides_matrix_test
      "board_with_two_boxes should have value of 1 at box (2,3)"
      big_board_with_two_boxes 2 3 1;
    sides_matrix_test
      "board_with_two_boxes should have value of 4 at box (3,3)"
      big_board_with_two_boxes 3 3 4;
    sides_matrix_test
      "board_with_two_boxes should have value of 2 at box (0,1)"
      big_board_with_two_boxes 0 1 2;
  ]

let command_tests =
  [
    (* Four corners then going either up or down *)
    valid_parse_test "top left down" "0 0 1 0" default_board
      [ 0; 0; 1; 0 ];
    valid_parse_test "bottom right up" "4 5 3 5" default_board
      [ 4; 5; 3; 5 ];
    valid_parse_test "bottom left up" "4 0 3 0" default_board
      [ 4; 0; 3; 0 ];
    valid_parse_test "top right down" "0 5 1 5" default_board
      [ 0; 5; 1; 5 ];
    (* Four sides then going either right or left *)
    valid_parse_test "top side right" "0 1 0 2" default_board
      [ 0; 1; 0; 2 ];
    valid_parse_test "bottom side left" "4 4 4 3" default_board
      [ 4; 4; 4; 3 ];
    valid_parse_test "left side right" "1 0 1 1" default_board
      [ 1; 0; 1; 1 ];
    valid_parse_test "right side left" "1 5 1 4" default_board
      [ 1; 5; 1; 4 ];
    (* in the middle*)
    valid_parse_test "middle up" "2 3 1 3" default_board [ 2; 3; 1; 3 ];
    valid_parse_test "middle down" "2 3 3 3" default_board
      [ 2; 3; 3; 3 ];
    valid_parse_test "middle left" "2 3 2 2" default_board
      [ 2; 3; 2; 2 ];
    valid_parse_test "middle right" "2 3 2 4" default_board
      [ 2; 3; 2; 4 ];
    valid_parse_test "middle down with spaces start" "    2 3 3 3"
      default_board [ 2; 3; 3; 3 ];
    valid_parse_test "middle down with spaces inbetween" "2   3  3   3"
      default_board [ 2; 3; 3; 3 ];
    valid_parse_test "middle down with spaces end" "2 3 3 3   "
      default_board [ 2; 3; 3; 3 ];
    (* Failing tests *)
    fail_parse_test "negative bounds" "-1 0 1 0" default_board Illegal;
    fail_parse_test "non numbers in input" "words" default_board Illegal;
    fail_parse_test "too far away" "0 0 2 0" default_board Illegal;
    fail_parse_test "diagonal move " "0 0 1 1" default_board Illegal;
    fail_parse_test "too far diagonal move " "0 0 2 2" default_board
      Illegal;
    fail_parse_test "list length more than 4" "2 3 3 3 3" default_board
      Illegal;
    fail_parse_test "Same point" "1 1 1 1" default_board Illegal;
    fail_parse_test "all negative bounds" "-1 -1 -2 -4" default_board
      Illegal;
  ]

let list_to_tuple lst =
  match lst with
  | [ x; y; z; d ] -> ((x, y), (z, d))
  | _ -> failwith "unimp"

let on_edge board move =
  let move_point_lst = String.split_on_char ' ' move in
  let move_filtered_lst =
    List.filter (fun a -> a <> "") move_point_lst
  in
  let tuple_move = list_to_tuple move_filtered_lst in
  (* If the move is on the edge/corner then add it to edge array else go
     to next element in array *)
  if
    (* Checking left, right, top, bottom sides *)
    let r1 = int_of_string (fst (fst tuple_move)) in
    let c1 = int_of_string (snd (fst tuple_move)) in
    let r2 = int_of_string (fst (snd tuple_move)) in
    let c2 = int_of_string (snd (snd tuple_move)) in
    let board_r = fst (Board.dimensions board) in
    let board_c = fst (Board.dimensions board) in
    (c1 = 0 && c2 = 0)
    || (c1 = board_c && c2 = board_c)
    || (r1 = 0 && r2 = 0)
    || (r1 = board_r && r2 = board_r)
  then true
  else false

let medium_bot_test
    (name : string)
    (board : Board.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output (on_edge board (Ai.medium board))

let medium_bot_complete_box_test
    (name : string)
    (board : Board.t)
    (expected_output : string) : test =
  name >:: fun _ -> assert_equal expected_output (Ai.medium board)

let board_with_no_edge_moves =
  update_board
    ((2, 1), (2, 2))
    Blue
    (update_board
       ((2, 0), (2, 1))
       Red
       (update_board
          ((0, 1), (0, 2))
          Blue
          (update_board
             ((0, 0), (0, 1))
             Red
             (update_board
                ((1, 2), (2, 2))
                Blue
                (update_board
                   ((0, 2), (1, 2))
                   Red
                   (update_board
                      ((1, 0), (2, 0))
                      Blue
                      (update_board
                         ((0, 0), (1, 0))
                         Red
                         (make_board (2, 2)))))))))

let board_complete_one_box_5x5 =
  update_board
    ((1, 2), (1, 3))
    Red
    (update_board
       ((2, 2), (2, 3))
       Blue
       (update_board ((1, 2), (2, 2)) Red (make_board (5, 5))))

let is_not_third_move_box row col board move =
  let empty_sides_list = empty_sides row col board in
  if
    List.nth empty_sides_list 0 <> move
    && List.nth empty_sides_list 1 <> move
  then true
  else false

let hard_bot_not_complete_box_test
    (name : string)
    (row : int)
    (col : int)
    (board : Board.t)
    (expected_output : bool) : test =
  name >:: fun _ ->
  assert_equal expected_output
    (is_not_third_move_box row col board (Ai.hard board))

let board_box_half_filled_5x5 =
  update_board
    ((2, 2), (2, 3))
    Blue
    (update_board ((1, 2), (2, 2)) Red (make_board (5, 5)))

let ai_tests =
  [
    medium_bot_test "blank board, move should be on the edge"
      default_board true;
    medium_bot_test "board with no edge moves available"
      board_with_no_edge_moves false;
    medium_bot_complete_box_test
      "board with one box to be completed- should priortize the box \
       and fill it in"
      board_complete_one_box_5x5 "1 3 2 3";
    hard_bot_not_complete_box_test "box with 2 moves available" 2 3
      board_box_half_filled_5x5 true;
  ]

let suite =
  "test suite for final project"
  >::: List.flatten
         [
           player_tests;
           board_tests;
           command_tests;
           dimensions_tests;
           end_game_tests;
           score_tests;
           side_matrix_tests;
           ai_tests;
         ]

let _ = run_test_tt_main suite
