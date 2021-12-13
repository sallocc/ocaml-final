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

let abc =
  Pos_grams.ngrams 4 1
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7)]

let def =
  Pos_grams.ngrams 2 2
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7); (0, 8); (0, 9) ]

let winer history board = 
  match history_to_board history board |> (is_game_over []) with 
  | true,p -> p
  | false,_ -> 0

let record_distribution n player history = 
  Pos_grams.ngrams n player history


(* a random move from distribution *)
(* if not valid move up if still not re draw *)



(* let () =
  standard_distribution
  failwith "bad" *)
(* let () = correct_directory 1 ; write_sexp_to_file "abc.txt" abc *)
(* let dis = sexp_to_map "abc.txt" *)

