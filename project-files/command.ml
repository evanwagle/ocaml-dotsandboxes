open Board

type result =
  | Legal of int list
  | Illegal

(* Splits the user's input into an list, with any leading, trailing,
   inbetween spaces removed.*)
let split_list s =
  let str = String.split_on_char ' ' s in
  List.filter (fun a -> a <> "") str

(** Checks if s can be represented as int*)
let is_int s = try int_of_string s with Failure f -> -1

(** Checks if every member of list can be represented as the zero int or
    a positive int.*)
let rec valid_list int_list =
  match int_list with
  | [] -> true
  | h :: t -> if h < 0 then false else valid_list t

(* Assumes that 1 was already added to row and column to account for
   dots vs boxes board size *)
let in_bounds int_list r c =
  if List.nth int_list 0 > r || List.nth int_list 2 > r then false
  else if List.nth int_list 1 > c || List.nth int_list 3 > c then false
  else true

(* ----- Helper functions for valid move ----- *)
let top_left r2 c2 =
  if (r2 = 1 && c2 = 0) || (r2 = 0 && c2 = 1) then true else false

let bottom_right r1 c1 r2 c2 =
  if (c1 - 1 = c2 && r1 = r2) || (c1 = c2 && r1 - 1 = r2) then true
  else false

let bottom_left r1 c1 r2 c2 =
  if (c1 = c2 && r1 - 1 = r2) || (c1 + 1 = c2 && r1 = r2) then true
  else false

let top_right r1 c1 r2 c2 =
  if (c1 - 1 = c2 && r1 = r2) || (c1 = c2 && r1 + 1 = r2) then true
  else false

let top_side c1 r2 c2 =
  if
    (r2 = 0 && c1 - 1 = c2)
    || (r2 = 1 && c1 = c2)
    || (r2 = 0 && c1 + 1 = c2)
  then true
  else false

let bottom_side r1 c1 r2 c2 =
  if
    (r1 = r2 && c1 - 1 = c2)
    || (r1 - 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 + 1 = c2)
  then true
  else false

let left_side r1 r2 c2 =
  if
    (r1 - 1 = r2 && c2 = 0)
    || (r1 = r2 && c2 = 1)
    || (r1 + 1 = r2 && c2 = 0)
  then true
  else false

let right_side r1 c1 r2 c2 =
  if
    (r1 - 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 - 1 = c2)
    || (r1 + 1 = r2 && c1 = c2)
  then true
  else false

let middle r1 c1 r2 c2 =
  if
    (r1 - 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 - 1 = c2)
    || (r1 + 1 = r2 && c1 = c2)
    || (r1 = r2 && c1 + 1 = c2)
  then true
  else false

(* Takes in valid_list and checks if the user's input is a valid move.
   Valid moves are within the board's boundaries and are directly 1
   point up, down, left right, from the initial point *)
let valid_move int_list r c =
  let r1 = List.nth int_list 0 in
  let c1 = List.nth int_list 1 in
  let r2 = List.nth int_list 2 in
  let c2 = List.nth int_list 3 in
  (* Checks general bounds *)
  if in_bounds int_list r c = false then false (* Checks top left*)
  else if r1 = 0 && c1 == 0 then top_left r2 c2
  else if r1 = r && c1 = c then bottom_right r1 c1 r2 c2
  else if r1 = r && c1 = 0 then bottom_left r1 c1 r2 c2
  else if r1 = 0 && c1 = c then top_right r1 c1 r2 c2
  else if r1 = 0 then top_side c1 r2 c2
  else if r1 = r then bottom_side r1 c1 r2 c2
  else if c1 = 0 then left_side r1 r2 c2
  else if c1 = c then right_side r1 c1 r2 c2
  else middle r1 c1 r2 c2

let parse s board =
  let s_list = split_list s in
  let int_list = List.map is_int s_list in
  if List.length int_list <> 4 then Illegal
  else if valid_list int_list = false then Illegal
  else
    let rc = Board.dimensions board in
    match rc with
    | r, c ->
        if valid_move int_list r c = false then Illegal
        else Legal int_list
