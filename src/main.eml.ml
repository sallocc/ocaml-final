open Core
open Board
open Lib

let tempBoard = ref [[]]

let winner = ref 0

let board = [[1; 0; 1; 1; 1; 1; 1];
                      [1; 0; 1; 1; 0; 1; 0];
                      [1; 0; 1; 1; 1; 1; 1];
                      [1; 0; 1; 1; 1; 1; 1];
                      [1; 0; 0; 1; 1; 1; 1];
                      [1; 0; 1; 1; 1; 1; 1]]

let emptyBoard = [[0; 0; 0; 0; 0; 0; 0];
                      [0; 0; 0; 0; 0; 0; 0];
                      [0; 0; 0; 0; 0; 0; 0];
                      [0; 0; 0; 0; 0; 0; 0];
                      [0; 0; 0; 0; 0; 0; 0];
                      [0; 0; 0; 0; 0; 0; 0]]

let show_form ?message request =
  <html>
  <body>
%   let rec displayRow row =
%     match row with
%     | [] -> 
        <span>|</span></br>
%     | 0 :: tl -> 
        <span style="margin: 0 auto" >| --</span>
%     displayRow tl
%     | 1 :: tl -> 
        <span style="margin: 0 autor" >| O </span>
%     displayRow tl
%     | 2 :: tl -> 
        <span style="margin: 0 auto" >| X </span>
%     displayRow tl
%     | _ :: tl -> 
        <span style="margin: 0 auto" >| ! </span>
%     displayRow tl
%   in
%   let rec displayTable gameboard =
%   match gameboard with
%   | hd :: tl -> displayRow hd; displayTable tl;
%   | [] -> 
        </br>
%   in
%   begin match message with
%   | None -> ()
%   | Some message ->
%       displayTable message
%   end;
%   begin match winner.contents with
%   | 0 ->
      <%s! Dream.form_tag ~action:"/" request %>
        <p>Enter your move:</p></br>
        <input name="move" autofocus>
      </form> 
%   | _ ->
      <p>Player <%i winner.contents %> is the <b>winner</b>!!!</p>
%   end;
    </body> 
    </html>

let () =
  Dream.run
  @@ Dream.logger
  @@ Dream.memory_sessions
  @@ Dream.router [

    Dream.get  "/"
      (fun request ->
        tempBoard := emptyBoard;
        setPlayer 1;
        Dream.html (show_form ~message:(List.rev tempBoard.contents) request));

    Dream.post "/"
      (fun request ->
        match%lwt Dream.form request with
        | `Ok ["move", move] ->
          let moveCol = int_of_string move in
          if moveCol <= 6 && moveCol >= 0 then
          (match makeMove tempBoard.contents moveCol with
          | None -> ()
          | Some newBoard -> tempBoard := newBoard; 
          let gameOver, gameWinner = is_game_over newBoard gameHistory.contents in
          (match gameOver with
          | false -> winner := winner.contents;
          | true -> winner := gameWinner;);
          if currPlayer.contents = 1 then setPlayer 2
          else setPlayer 1;)
          else tempBoard := tempBoard.contents;
          Dream.html (show_form ~message:(List.rev tempBoard.contents) request)
        | _ ->
          Dream.empty `Bad_Request);

  ]
  @@ Dream.not_found
