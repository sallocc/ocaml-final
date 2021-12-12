open Core
open Lib 
(* open Board  *)
module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end

module Pos_grams = N_grams (Pos)
module Pos_list = List_key (Pos)
module Pos_List_Map = Map.Make (Pos_list)

let write_sexp_to_file filename text =
  let output_file = Out_channel.create ~append:true filename in
  Out_channel.output_string output_file (Pos_List_Map.sexp_of_t Pos.sexp_of_t text |> Sexp.to_string);
  Out_channel.close output_file

let sexp_to_map filename = 
  Pos_List_Map.t_of_sexp Pos.t_of_sexp (Sexp.load_sexp filename);;

let abc = Pos_grams.ngrams 1 2 [];;
  
(* change to correct directory *)
(* let correct_directory _ = Sys.chdir("/Users/zhenboyan/Desktop") *)
(* let correct_directory _ = Sys.chdir("/Users/zhenboyan/Desktop/ocaml-final") *)

