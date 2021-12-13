open Core
open Lib 
(* open Board  *)
module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end
module Pos_grams = N_grams (Pos)

module Dist = struct 
  type t = Pos_grams.distribution [@@deriving sexp]
end 
module Write_Distribution = Dist

let correct_directory _ = Sys.chdir("/Users/zhenboyan/Desktop/ocaml-final")

let write_sexp_to_file filename distribution =
  let output_file = Out_channel.create ~append:false filename in
  Out_channel.output_string output_file (Write_Distribution.sexp_of_t distribution |> Sexp.to_string);
  Out_channel.close output_file 

let sexp_to_map filename = 
  Write_Distribution.t_of_sexp (Sexp.load_sexp filename)

let abc = Pos_grams.ngrams 2 1 [(0,1); (0,2); (0,3); (0,4); (0,5); (0,6); (0,7); (0,8); (0,9)]
(* let dis = sexp_to_map "abc.txt" *)
let () = correct_directory 1 ; write_sexp_to_file "abc.txt" abc;

(* let abc = Pos_grams.ngrams 2 1 [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ] *)
  
(* change to correct directory *)
(* let correct_directory _ = Sys.chdir("/Users/zhenboyan/Desktop") *)
(* let correct_directory _ = Sys.chdir("/Users/zhenboyan/Desktop/ocaml-final") *)

