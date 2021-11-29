Open Core;;

(* THE BOTTOM LEFT OF THE BOARD IS (0,0) *)

type gameboard = int list list

type move = int

let currPlayer = ref 1;


(* Given a board and an (x,y) position, it will return the value at
that position in the board. *)
let getPos (xPos: int) (yPos: int) (board: gameboard): int = 
    if xPos < 0 || xPos > 6 || yPos < 0 || yPos > 6 then failwith "Invalid position" else
    let firstRows, laterRows = List.split_n board yPos in
    let currRow = List.hd_exn laterRows in
    let firstPositions, laterPositions = List.split_n currRow xPos in
    List.hd_exn laterPositions;;

let rec changePos (currX: int) (currY: int) (xPos: int) (yPos: int) (newVal: int) (board: gameboard): gameboard =
    if xPos < 0 || xPos > 6 || yPos < 0 || yPos > 6 then failwith "Invalid position" else
    match board with
    | hd :: tl -> if currY < yPos then hd :: (changePos (currX + 1) (currY + 1) xPos yPos newVal tl)
                  else (changeRow xPos newVal hd) :: tl
    | [] -> failwith "Invalid board";;


let changeRow (xPos: int) (newVal: int) (row: int list): int list =
    let firstPositions, laterPositions = List.split_n row xPos in
    match laterPositions with
    | [] -> failwith "Invalid position"
    | hd :: tl -> List.append firstPositions (newVal :: tl);;

(* Given a board and a column to place a piece, it returns None if the move is
invalid, and otherwise returns the updated board with the lowest available position
in the column provided filled. *)
let makeMove (board: gameboard) (col: move): gameboard Option =
    if getPos col 6 board <> 0 then None else
    let x, y = getAvailableSpace board col 0 in
    Some (changePos 0 0 x y !currPlayer board);;

let rec getAvailableSpace (board: gameboard) (col: move) (currRow: int): int * int =
    if getPos col currRow board = 0 then (col, currRow) else
    getAvailableSpace board col (currRow + 1)

let isGameOver (board: gameboard): bool * int =
    if gameOverHelp board 1 then (true, 1) else
    if gameOverHelp board 2 then (true, 2) else
    (false, 0);;

let gameOverHelp (board: gameboard) (player: int): bool =