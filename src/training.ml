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

let empty_d = Pos_grams.ngrams 1 1 []

let store = "dist.txt"

let original_dist = sexp_to_map store

let gram = 4

let num_games = 1000000

let random_d = random_distribution

let repeat = 10

let write_string_to_file filename text =
  let outc = Out_channel.create ~append:false filename in
  Out_channel.output_string outc text;
  Out_channel.close outc

let vs_random store_file =
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          write_string_to_file "hist.txt"
            (List.sexp_of_t Pos.sexp_of_t history |> Sexp.to_string);
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          play new_dist (history @ [ ai_dist_move random_d history ]) i
    else write_sexp_to_file store_file new_dist
  in
  play original_dist [] 0

let vs_itself store_file =
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          write_string_to_file "hist.txt"
            (List.sexp_of_t Pos.sexp_of_t history |> Sexp.to_string);
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          play new_dist
            (history @ [ ai_move history gram new_dist random_d repeat ])
            i
    else write_sexp_to_file store_file new_dist
  in
  play original_dist [] 0

let train_player1 player1 =
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          write_string_to_file "hist.txt"
            (List.sexp_of_t Pos.sexp_of_t history |> Sexp.to_string);
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          if List.length history % 2 = 0 then
            play new_dist
              (history @ [ ai_move history gram new_dist random_d repeat ])
              i
          else play new_dist (history @ [ ai_dist_move random_d history ]) i
    else write_sexp_to_file player1 new_dist
  in
  play original_dist [] 0

let train_player2 player2 =
  let rec play new_dist history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play new_dist [] i
      | true, p ->
          write_string_to_file "hist.txt"
            (List.sexp_of_t Pos.sexp_of_t history |> Sexp.to_string);
          play
            (merge_distribution (Pos_grams.ngrams gram p history) new_dist)
            [] (i + 1)
      | false, _ ->
          if List.length history % 2 = 1 then
            play new_dist
              (history @ [ ai_move history gram new_dist random_d repeat ])
              i
          else play new_dist (history @ [ ai_dist_move random_d history ]) i
    else write_sexp_to_file player2 new_dist
  in
  play original_dist [] 0

let train_both player1 player2 =
  let rec play p1 p2 history i =
    switch_player history;
    if i < num_games then
      match is_game_over (history_to_board history) history with
      | true, -1 -> play p1 p2 [] i
      | true, 1 ->
          write_string_to_file "hist.txt"
            (List.sexp_of_t Pos.sexp_of_t history |> Sexp.to_string);
          play
            (merge_distribution (Pos_grams.ngrams gram 1 history) p1) p2
            [] (i + 1)
      | true, _ ->
          play p1
            (merge_distribution (Pos_grams.ngrams gram 2 history) p2)
            [] (i + 1)
      | false, _ ->
          if List.length history % 2 = 1 then
            play p1 p2
              (history @ [ ai_move history gram p2 random_d repeat ])
              i
          else
            play p1 p2
              (history @ [ ai_move history gram p1 random_d repeat ])
              i
    else write_sexp_to_file player2 p2;
    write_sexp_to_file player1 p1
  in
  play (sexp_to_map "player1.txt") (sexp_to_map "player2.txt") [] 0

let () =
  (* write_sexp_to_file ("dist.txt") (Pos_grams.ngrams 1 1 []); *)
  correct_directory "set to the right directory";
  train_player1 "player1.txt"