type color =
  | Blank
  | Red
  | Blue

type point = {
  up : color option;
  down : color option;
  left : color option;
  right : color option;
}

type t = {
  board : point array array;
  mutable score : int * int;
  full : bool;
  dim : int * int;
  mutable last_filled : (int * int) list;
  mutable branches_remaining : int;
  sides_matrix : int array array;
}

type direc =
  | Left
  | Right
  | Up
  | Down

exception Out_of_Board

let make_board size =
  assert (fst size > 0);
  assert (snd size > 0);
  let rows = fst size + 1 in
  let cols = snd size + 1 in
  let handle_edges arr =
    Array.iter (fun x -> x.(0) <- { (x.(0)) with up = None }) arr;
    Array.iter
      (fun x -> x.(cols - 1) <- { (x.(cols - 1)) with down = None })
      arr;
    arr.(0) <- Array.map (fun x -> { x with left = None }) arr.(0);
    arr.(rows - 1) <-
      Array.map (fun x -> { x with right = None }) arr.(rows - 1)
  in
  let a =
    Array.make_matrix rows cols
      {
        up = Some Blank;
        down = Some Blank;
        left = Some Blank;
        right = Some Blank;
      }
  in
  handle_edges a;
  {
    board = a;
    score = (0, 0);
    full = false;
    dim = (rows - 1, cols - 1);
    last_filled = [];
    branches_remaining = (rows * (cols - 1)) + ((rows - 1) * cols);
    sides_matrix = Array.make_matrix (rows - 1) (cols - 1) 0;
  }

let direction x1 x2 y1 y2 =
  if x2 - x1 = 1 then Right
  else if x1 - x2 = 1 then Left
  else if y2 - y1 = 1 then Down
  else Up

let get_branch points board =
  let brd = board.board in
  let point1, point2 = (fst points, snd points) in
  let x1, y1 = (fst point1, snd point1) in
  let x2, y2 = (fst point2, snd point2) in
  let d = direction x1 x2 y1 y2 in
  let color_opt =
    match d with
    | Left -> brd.(x1).(y1).left
    | Right -> brd.(x1).(y1).right
    | Up -> brd.(x1).(y1).up
    | Down -> brd.(x1).(y1).down
  in
  match color_opt with Some c -> c | None -> raise Out_of_Board

let dimensions board = board.dim

let branch_filled points board =
  match get_branch points board with
  | Red | Blue -> true
  | Blank -> false

let update_score_and_filled b color x y =
  b.last_filled <- (x, y) :: b.last_filled;
  match color with
  | Red -> b.score <- (fst b.score + 1, snd b.score)
  | Blue -> b.score <- (fst b.score, snd b.score + 1)
  | Blank -> failwith "What the hell did you do?"

let rec update_board points color board =
  board.last_filled <- [];
  let brd = board.board in
  let point1, point2 = (fst points, snd points) in
  let x1, y1 = (fst point1, snd point1) in
  let x2, y2 = (fst point2, snd point2) in
  let p1_direc = direction x1 x2 y1 y2 in
  let p2_direc =
    match p1_direc with
    | Left -> Right
    | Right -> Left
    | Up -> Down
    | Down -> Up
  in
  let update_point x y = function
    | Left -> brd.(x).(y) <- { (brd.(x).(y)) with left = Some color }
    | Right -> brd.(x).(y) <- { (brd.(x).(y)) with right = Some color }
    | Down -> brd.(x).(y) <- { (brd.(x).(y)) with down = Some color }
    | Up -> brd.(x).(y) <- { (brd.(x).(y)) with up = Some color }
  in
  let check_box x y = function
    | Right -> check_right x y board color
    | Left -> check_left x y board color
    | Up -> check_up x y board color
    | Down -> check_down x y board color
  in
  update_point x1 y1 p1_direc;
  update_point x2 y2 p2_direc;
  check_box x1 y1 p1_direc;
  update_sides_matrix x1 y1 board p1_direc;
  board.branches_remaining <- board.branches_remaining - 1;
  if board.branches_remaining = 0 then { board with full = true }
  else board

and check_left x y board color =
  if not (y - 1 < 0) then
    if
      branch_filled ((x, y), (x, y - 1)) board
      && branch_filled ((x, y - 1), (x - 1, y - 1)) board
      && branch_filled ((x - 1, y - 1), (x - 1, y)) board
    then update_score_and_filled board color (x - 1) (y - 1);
  if not (y + 1 > fst (dimensions board)) then
    if
      branch_filled ((x, y), (x, y + 1)) board
      && branch_filled ((x, y + 1), (x - 1, y + 1)) board
      && branch_filled ((x - 1, y + 1), (x - 1, y)) board
    then update_score_and_filled board color (x - 1) y

and check_right x y board color =
  if not (y - 1 < 0) then
    if
      branch_filled ((x, y), (x, y - 1)) board
      && branch_filled ((x, y - 1), (x + 1, y - 1)) board
      && branch_filled ((x + 1, y - 1), (x + 1, y)) board
    then update_score_and_filled board color x (y - 1);
  if not (y + 1 > fst (dimensions board)) then
    if
      branch_filled ((x, y), (x, y + 1)) board
      && branch_filled ((x, y + 1), (x + 1, y + 1)) board
      && branch_filled ((x + 1, y + 1), (x + 1, y)) board
    then update_score_and_filled board color x y

and check_up x y board color =
  if not (x - 1 < 0) then
    if
      branch_filled ((x, y), (x - 1, y)) board
      && branch_filled ((x - 1, y), (x - 1, y - 1)) board
      && branch_filled ((x - 1, y - 1), (x, y - 1)) board
    then update_score_and_filled board color (x - 1) (y - 1);
  if not (x + 1 > snd (dimensions board)) then
    if
      branch_filled ((x, y), (x + 1, y)) board
      && branch_filled ((x + 1, y), (x + 1, y - 1)) board
      && branch_filled ((x + 1, y - 1), (x, y - 1)) board
    then update_score_and_filled board color x (y - 1)

and check_down x y board color =
  if not (x - 1 < 0) then
    if
      branch_filled ((x, y), (x - 1, y)) board
      && branch_filled ((x - 1, y), (x - 1, y + 1)) board
      && branch_filled ((x - 1, y + 1), (x, y + 1)) board
    then update_score_and_filled board color (x - 1) y;
  if not (x + 1 > snd (dimensions board)) then
    if
      branch_filled ((x, y), (x + 1, y)) board
      && branch_filled ((x + 1, y), (x + 1, y + 1)) board
      && branch_filled ((x + 1, y + 1), (x, y + 1)) board
    then update_score_and_filled board color x y

and update_sides_matrix x y board = function
  | Left -> update_left x y board
  | Right -> update_right x y board
  | Up -> update_up x y board
  | Down -> update_down x y board

and update_left x y board =
  if not (y - 1 < 0) then
    board.sides_matrix.(x - 1).(y - 1) <-
      board.sides_matrix.(x - 1).(y - 1) + 1;
  if not (y + 1 > fst (dimensions board)) then
    board.sides_matrix.(x - 1).(y) <- board.sides_matrix.(x - 1).(y) + 1

and update_right x y board =
  if not (y - 1 < 0) then
    board.sides_matrix.(x).(y - 1) <- board.sides_matrix.(x).(y - 1) + 1;
  if not (y + 1 > fst (dimensions board)) then
    board.sides_matrix.(x).(y) <- board.sides_matrix.(x).(y) + 1

and update_up x y board =
  if not (x - 1 < 0) then
    board.sides_matrix.(x - 1).(y - 1) <-
      board.sides_matrix.(x - 1).(y - 1) + 1;
  if not (x + 1 > snd (dimensions board)) then
    board.sides_matrix.(x).(y - 1) <- board.sides_matrix.(x).(y - 1) + 1

and update_down x y board =
  if not (x - 1 < 0) then
    board.sides_matrix.(x - 1).(y) <- board.sides_matrix.(x - 1).(y) + 1;
  if not (x + 1 > snd (dimensions board)) then
    board.sides_matrix.(x).(y) <- board.sides_matrix.(x).(y) + 1

let last_filled board = board.last_filled

let score board = board.score

let end_game board = board.full

let sides_matrix board = board.sides_matrix
