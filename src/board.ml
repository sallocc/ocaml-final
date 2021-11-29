Open Core;;

type gameboard = int list list

type move = int

let currPlayer = int ref;

let getPos (currX: int) (currY: int) (xPos: int) (yPos: int) (board: gameboard): int = 
    let firstRows, laterRows = List.split_n board yPos int


let makeMove (board: gameboard) (col: move): gameboard Option =


let isGameOver (board: gameboard): bool * int =
    if gameOverHelp board 1 then (true, 1) else
    if gameOverHelp board 2 then (true, 2) else
    (false, 0);;

let gameOverHelp (board: gameboard) (player: int): bool =