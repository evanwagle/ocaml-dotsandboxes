open Board

(** Quadrap*)
let valid_moves = ref [||]

let convert_string r1 c1 r2 c2 =
  string_of_int r1 ^ " " ^ string_of_int c1 ^ " " ^ string_of_int r2
  ^ " " ^ string_of_int c2 ^ " "

let convert_alt_string r1 c1 r2 c2 =
  string_of_int r2 ^ " " ^ string_of_int c2 ^ " " ^ string_of_int r1
  ^ " " ^ string_of_int c1 ^ " "

let get_empty_branch r1 c1 r2 c2 board =
  (* Points (r1, c1) and (r2, c2) *)
  let string_move = convert_string r1 c1 r2 c2 in

  (* Same move but represented as points (r2, c2) and (r1, c1) *)
  let alt_string_move = convert_alt_string r1 c1 r2 c2 in
  match Command.parse string_move board with
  | Legal _ -> (
      let tuple_move = ((r1, c1), (r2, c2)) in
      match Board.branch_filled tuple_move board with
      | true -> ()
      | false ->
          (* Checks if alt move is already added to valid_moves. Does
             not add if move is already in valid_move *)
          if
            Array.exists
              (fun x ->
                if x = string_move || x = alt_string_move then true
                else false)
              !valid_moves
          then ()
          else
            valid_moves := Array.append !valid_moves [| string_move |])
  | Illegal -> ()

let get_top r c =
  string_of_int r ^ " " ^ string_of_int c ^ " " ^ string_of_int r ^ " "
  ^ string_of_int (c + 1)

let get_bottom r c =
  string_of_int (r + 1)
  ^ " " ^ string_of_int c ^ " "
  ^ string_of_int (r + 1)
  ^ " "
  ^ string_of_int (c + 1)

let get_right r c =
  string_of_int r ^ " "
  ^ string_of_int (c + 1)
  ^ " "
  ^ string_of_int (r + 1)
  ^ " "
  ^ string_of_int (c + 1)

let get_left r c =
  string_of_int r ^ " " ^ string_of_int c ^ " "
  ^ string_of_int (r + 1)
  ^ " " ^ string_of_int c

let empty_sides r c board =
  let top = get_top r c in

  let bottom = get_bottom r c in

  let right = get_right r c in

  let left = get_left r c in

  let lst = ref [] in
  if not (branch_filled ((r, c), (r, c + 1)) board) then
    lst := top :: !lst;
  if not (branch_filled ((r, c), (r + 1, c)) board) then
    lst := left :: !lst;
  if not (branch_filled ((r + 1, c), (r + 1, c + 1)) board) then
    lst := bottom :: !lst;
  if not (branch_filled ((r, c + 1), (r + 1, c + 1)) board) then
    lst := right :: !lst;
  !lst

let get_empty_branches board =
  let dim = Board.dimensions board in
  for r1 = 0 to fst dim do
    for c1 = 0 to snd dim do
      for r2 = 0 to fst dim do
        for c2 = 0 to snd dim do
          get_empty_branch r1 c1 r2 c2 board
        done
      done
    done
  done

let easy board =
  get_empty_branches board;
  Random.self_init ();
  let rand_index = Random.int (Array.length !valid_moves) in
  let ai_move = Array.get !valid_moves rand_index in
  valid_moves := [||];
  ai_move

let list_to_tuple lst =
  match lst with
  | [ x; y; z; d ] -> ((x, y), (z, d))
  | _ -> failwith "unimp"

let edge_moves = ref [||]

let complete_box_moves = ref [||]

(* Searches sides_matrix for points which have 3 sides completed, if 3
   sides are completed, then the available move corresponding to the
   point is found. This move is added to complete_box_moves array. *)
let find_box_finish sides_matrix_board board =
  for row = 0 to Array.length sides_matrix_board - 1 do
    for col = 0 to Array.length (Array.get sides_matrix_board 0) - 1 do
      if Array.get (Array.get sides_matrix_board row) col = 3 then
        complete_box_moves :=
          Array.append !complete_box_moves
            [| List.hd (empty_sides row col board) |]
      else ()
    done
  done

let check_sides tuple_move board =
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

let get_edges board =
  for h = 0 to Array.length !valid_moves - 1 do
    (* Converts single move from array list to a tuple of tuples. *)
    let move_point_lst =
      String.split_on_char ' ' (Array.get !valid_moves h)
    in
    let move_filtered_lst =
      List.filter (fun a -> a <> "") move_point_lst
    in
    let tuple_move = list_to_tuple move_filtered_lst in

    (* If the move is on the edge/corner then add it to edge array else
       go to next element in array *)
    if
      (* Checking left, right, top, bottom sides *)
      check_sides tuple_move board
    then
      edge_moves :=
        Array.append !edge_moves [| Array.get !valid_moves h |]
    else ()
  done

let medium board =
  Random.self_init ();
  let sides_matrix_board = sides_matrix board in
  find_box_finish sides_matrix_board board;
  get_empty_branches board;
  get_edges board;
  (* First checks if there are boxes to be completed, then chooses a
     random move from the available boxes. Then checks if there are edge
     moves available and chooses a random edge move. Lastly, makes a
     random move once previous 2 strategies are unavailable. *)
  if Array.length !complete_box_moves > 0 then (
    let rand_index = Random.int (Array.length !complete_box_moves) in
    let ai_move = Array.get !complete_box_moves rand_index in
    complete_box_moves := [||];
    valid_moves := [||];
    ai_move)
  else if Array.length !edge_moves > 0 then (
    let rand_index = Random.int (Array.length !edge_moves) in
    let ai_move = Array.get !edge_moves rand_index in
    edge_moves := [||];
    valid_moves := [||];
    ai_move)
  else
    let rand_index = Random.int (Array.length !valid_moves) in
    let ai_move = Array.get !valid_moves rand_index in
    valid_moves := [||];
    ai_move

let bad_moves = ref [||]

let find_box_not_finish sides_matrix_board board =
  for row = 0 to Array.length sides_matrix_board - 1 do
    for col = 0 to Array.length (Array.get sides_matrix_board 0) - 1 do
      if Array.get (Array.get sides_matrix_board row) col = 2 then
        bad_moves :=
          Array.append !bad_moves
            (Array.of_list (empty_sides row col board))
      else ()
    done
  done

let debug = false

let hard board =
  if debug then medium board
  else
    let _ = () in
    Random.self_init ();
    let sides_matrix_board = sides_matrix board in
    (* Searches sides_matrix for points which have 3 sides completed, if
       3 sides are completed, then the available move corresponding to
       the point is found. This move is added to complete_box_moves
       array. *)
    find_box_finish sides_matrix_board board;
    (* Searches sides_matrix for boxes which have 2 sides completed to
       avoid creating a new box for the player to close. If there are 2
       sides are completed, then the available move corresponding to the
       point is found. This move is added to bad_moves array.*)
    find_box_not_finish sides_matrix_board board;
    get_empty_branches board;
    get_edges board;
    if Array.length !complete_box_moves > 0 then (
      let rand_index = Random.int (Array.length !complete_box_moves) in
      let ai_move = Array.get !complete_box_moves rand_index in
      bad_moves := [||];
      complete_box_moves := [||];
      valid_moves := [||];
      edge_moves := [||];
      ai_move)
    else
      let moves = Array.to_list !valid_moves in
      let good_moves =
        List.filter
          (fun x -> Array.exists (fun y -> y <> x) !bad_moves)
          moves
      in
      let best_moves =
        List.filter
          (fun x -> Array.exists (fun y -> y = x) !edge_moves)
          good_moves
      in
      if List.length best_moves > 0 then (
        let rand_index = Random.int (List.length best_moves) in
        let ai_move = List.nth best_moves rand_index in
        bad_moves := [||];
        valid_moves := [||];
        edge_moves := [||];
        ai_move)
      else if List.length good_moves > 0 then (
        let rand_index = Random.int (List.length good_moves) in
        let ai_move = List.nth good_moves rand_index in
        bad_moves := [||];
        valid_moves := [||];
        edge_moves := [||];
        ai_move)
      else
        let _ = () in
        bad_moves := [||];
        valid_moves := [||];
        edge_moves := [||];
        easy board
