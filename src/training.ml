open Core
open Lib
open Board

module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end

module Pos_grams = N_grams (Pos)

module Dist = struct
  type t = Pos_grams.distribution [@@deriving sexp]
end

module Write_Distribution = Dist

let correct_directory _ = Sys.chdir "/Users/zhenboyan/Desktop/ocaml-final"

let write_sexp_to_file filename distribution =
  let output_file = Out_channel.create ~append:false filename in
  Out_channel.output_string output_file
    (Write_Distribution.sexp_of_t distribution |> Sexp.to_string);
  Out_channel.close output_file

let sexp_to_map filename =
  Write_Distribution.t_of_sexp (Sexp.load_sexp filename)

let switch_player history =
  match List.length history % 2 with
  | 0 -> setPlayer 2
  | 1 -> setPlayer 1
  | _ -> ()

let ai_trainning_move (dist : int -> int) (history : (int * int) list) :
    int * int =
  if List.length history >= 42 then invalid_arg "invalid history"
  else
    let m = dist 0 in
    let rec until_valid level move repeat =
      if level < 6 then
        if level = 4 && repeat <> 20 then until_valid 0 (dist repeat) (repeat + 1)
        else if ai_is_valid_move (level, move) history then (level, move)
        else until_valid (level + 1) move repeat
      else until_valid 0 (dist 1) repeat
    in
    until_valid 0 m 0

let all_0 _ : int = distribution_maker 94 1 1 1 1 1 1

let all_1 _ : int = distribution_maker 1 94 1 1 1 1 1

let all_2 _ : int = distribution_maker 1 1 94 1 1 1 1

let all_3 _ : int = distribution_maker 1 1 1 94 1 1 1

let all_4 _ : int = distribution_maker 1 1 1 1 94 1 1

let all_5 _ : int = distribution_maker 1 1 1 1 1 94 1

let all_6 _ : int = distribution_maker 1 1 1 1 1 1 94
let tail _ : int = distribution_maker 48 1 1 1 1 1 47
let left _ : int = distribution_maker 30 36 30 1 1 1 1
let right _ : int = distribution_maker 1 1 1 1 30 36 30 
let ll _ : int = distribution_maker 1 30 36 30 1 1 1
let rr _ : int = distribution_maker 1 1 1 30 36 30 1
let middle _ : int = distribution_maker 1 1 30 36 30 1 1
let lll _ : int = distribution_maker 47 48 1 1 1 1 1
let rrr _ : int = distribution_maker 1 1 1 1 1 48 47

let empty_d = Pos_grams.ngrams 1 1 []

let num_games = 500

let random_d = all_4

let repeat = 15

let write_string_to_file filename text =
  let outc = Out_channel.create ~append:false filename in
  Out_channel.output_string outc text;
  Out_channel.close outc

let vs_random store_file gram=
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          play new_dist (history @ [ ai_dist_move random_d history ]) i
    else write_sexp_to_file store_file new_dist
  in
  play (sexp_to_map store_file) [] 0

let vs_itself store_file gram =
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          play new_dist
            (history @ [ ai_move history gram new_dist random_d repeat ])
            i
    else write_sexp_to_file store_file new_dist
  in
  play (sexp_to_map store_file) [] 0

let train_player1 player1 gram =
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          if List.length history % 2 = 0 then
            play new_dist
              (history @ [ ai_move history gram new_dist random_d repeat ])
              i
          else
            play new_dist (history @ [ ai_trainning_move random_d history ]) i
    else write_sexp_to_file player1 new_dist
  in
  play (sexp_to_map "player1.txt") [] 0

let rec n_times n = 
  (train_player1 "player1.txt" n);
  if n > 3 then n_times (n-1)  else ()

let () =
  n_times 11
  (* correct_directory "set to the right directory"; *)
  