open Core

(* THE BOTTOM LEFT OF THE BOARD IS (0,0) *)

type gameboard = int list list

let empty =
  [
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let currPlayer = ref 1

let setPlayer (player : int) = currPlayer := player

type history = int * int list

let gameHistory = ref []
(* history is :: to the front, List.rev before giving to AI *)

(* Given a board and an (x,y) position, it will return the value at
   that position in the board. *)
let getPos (xPos : int) (yPos : int) (board : gameboard) : int =
  if xPos < 0 || xPos > 6 || yPos < 0 || yPos > 5 then
    failwith "Invalid position"
  else
    let _, laterRows = List.split_n board yPos in
    let currRow = List.hd_exn laterRows in
    let _, laterPositions = List.split_n currRow xPos in
    List.hd_exn laterPositions

let changeRow (xPos : int) (newVal : int) (row : int list) : int list =
  let firstPositions, laterPositions = List.split_n row xPos in
  match laterPositions with
  | [] -> failwith "Invalid position"
  | _ :: tl -> List.append firstPositions (newVal :: tl)

let rec changePos (currX : int) (currY : int) (xPos : int) (yPos : int)
    (newVal : int) (board : gameboard) : gameboard =
  if xPos < 0 || xPos > 6 || yPos < 0 || yPos > 5 then
    failwith "Invalid position"
  else
    match board with
    | hd :: tl ->
        if currY < yPos then
          hd :: changePos (currX + 1) (currY + 1) xPos yPos newVal tl
        else changeRow xPos newVal hd :: tl
    | [] -> failwith "Invalid board"

(* Given a board and a column to place a piece, it returns None if the move is
   invalid, and otherwise returns the updated board with the lowest available position
   in the column provided filled. *)
let rec getAvailableSpace (board : gameboard) (col : int) (currRow : int) :
    int * int =
  if getPos col currRow board = 0 then (col, currRow)
  else getAvailableSpace board col (currRow + 1)

(* history is updated as well*)
let makeMove (board : gameboard) (col : int) : gameboard option =
  if getPos col 5 board <> 0 then None
  else
    let x, y = getAvailableSpace board col 0 in
    gameHistory := (x, y) :: !gameHistory;
    Some (changePos 0 0 x y !currPlayer board)

let checkWin (board : gameboard) : bool * int =
  let horizontal board =
    List.fold board ~init:false ~f:(fun acc x ->
        acc
        ||
        match
          List.fold x ~init:0 ~f:(fun acc y ->
              if acc = 4 then 4 else if y = !currPlayer then acc + 1 else 0)
        with
        | 4 -> true
        | _ -> false)
  in
  let vertical board =
    let c = [ 0; 1; 2; 3; 4; 5; 6 ] in
    List.fold c ~init:false ~f:(fun acc i ->
        acc
        ||
        match
          List.fold board ~init:0 ~f:(fun acc x ->
              if acc = 4 then 4
              else if List.nth_exn x i = !currPlayer then acc + 1
              else 0)
        with
        | 4 -> true
        | _ -> false)
  in
  let diagonal1 board =
    List.foldi board ~init:false ~f:(fun i acc y ->
        if i < 3 then acc
        else
          acc
          || List.foldi y ~init:false ~f:(fun j acc2 x ->
                 if j > 3 then acc2
                 else
                   acc2
                   || x = !currPlayer
                      && x = List.nth_exn (List.nth_exn board (i - 1)) (j + 1)
                      && x = List.nth_exn (List.nth_exn board (i - 2)) (j + 2)
                      && x = List.nth_exn (List.nth_exn board (i - 3)) (j + 3)))
  in
  let diagonal2 board =
    List.foldi board ~init:false ~f:(fun i acc y ->
        if i > 2 then acc
        else
          acc
          || List.foldi y ~init:false ~f:(fun j acc2 x ->
                 if j > 3 then acc2
                 else
                   acc2
                   || x = !currPlayer
                      && x = List.nth_exn (List.nth_exn board (i + 1)) (j + 1)
                      && x = List.nth_exn (List.nth_exn board (i + 2)) (j + 2)
                      && x = List.nth_exn (List.nth_exn board (i + 3)) (j + 3)))
  in
  ( horizontal board || vertical board || diagonal1 board || diagonal2 board,
    !currPlayer )

(* -1 if it's draw else 1 or 2 *)
let is_game_over board h =
  match (List.length h, checkWin board) with
  | 42, (false, _) -> (true, -1)
  | _, z -> z

let history_to_board history =
  List.foldi history ~init:empty ~f:(fun i acc (j, k) ->
      List.mapi acc ~f:(fun i1 x ->
          if i1 = j then
            List.mapi x ~f:(fun i2 y -> if i2 = k then (i % 2) + 1 else y)
          else x))
