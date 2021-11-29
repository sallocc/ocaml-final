Open Core;;

type gameboard = int list list

type move = int

let currPlayer = 1 ref;

let getPos (xPos: int) (yPos: int) (board: gameboard): int = 
    let firstRows, laterRows = List.split_n board yPos in
    let currRow = List.hd_exn laterRows in
    let firstPositions, laterPositions = List.split_n currRow xPos in
    List.hd_exn laterPositions;;



let makeMove (board: gameboard) (col: move): gameboard Option =


let isGameOver (board: gameboard): bool * int =
    if gameOverHelp board 1 then (true, 1) else
    if gameOverHelp board 2 then (true, 2) else
    (false, 0);;

let gameOverHelp (board: gameboard) (player: int): bool =