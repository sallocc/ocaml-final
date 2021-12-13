open Core
open OUnit2
open Lib
open Board

(* OLD Lib Test *)
let a_list = [ "a"; "b"; "c"; "d"; "e" ]

let test_invalid_chunks _ =
  let invalid_chunks _ = chunks 0 a_list in
  assert_raises (Invalid_argument "invalid input n") invalid_chunks;
  let invalid_chunks' _ = chunks (-1) a_list in
  assert_raises (Invalid_argument "invalid input n") invalid_chunks';
  let invalid_chunks'' _ = chunks (-2) a_list in
  assert_raises (Invalid_argument "invalid input n") invalid_chunks''

let test_split_last _ =
  assert_equal ("e", [ "a"; "b"; "c"; "d" ])
  @@ split_last [ "a"; "b"; "c"; "d"; "e" ];
  assert_equal ("d", [ "a"; "b"; "c" ]) @@ split_last [ "a"; "b"; "c"; "d" ];
  assert_equal ("c", [ "a"; "b" ]) @@ split_last [ "a"; "b"; "c" ];
  assert_equal ("b", [ "a" ]) @@ split_last [ "a"; "b" ];
  assert_equal ("a", []) @@ split_last [ "a" ]

let test_invalid_split_last _ =
  let invalid_split_last _ = split_last [] in
  assert_raises (Invalid_argument "empty list") invalid_split_last

let exercise1_2_test =
  "Exercise 1 and 2"
  >: test_list
       [
         "invalid_chunks" >:: test_invalid_chunks;
         "split_last" >:: test_split_last;
         "invalid_split_last" >:: test_invalid_split_last;
       ]

module N = N_grams (Int)
module Token_list = List_key (Int)

let example_l = [ 1; 2; 3; 4; 5; 6; 7; 8; 9 ]

let bag_to_list (b : 'a Bag.t) (l : 'a list) =
  Bag.fold b ~init:l ~f:(fun acc a -> a :: acc)

let fold_sample_test n player l =
  List.fold
    (N.ngrams n player l |> Map.data)
    ~init:[]
    ~f:(fun acc a -> acc @ [ bag_to_list a [] ])
(* let key_sample_test n player l =
   (N.ngrams n player l |> Map.keys) *)

let test_ngrams _ =
  assert_equal [ [ 1; 3; 5; 7; 9 ] ] @@ fold_sample_test 1 1 example_l;
  assert_equal [ [ 2; 4; 6; 8 ] ] @@ fold_sample_test 1 2 example_l;
  assert_equal [ [ 3 ]; [ 5 ]; [ 7 ]; [ 9 ] ] @@ fold_sample_test 2 1 example_l;
  assert_equal [ [ 2 ]; [ 4 ]; [ 6 ]; [ 8 ] ] @@ fold_sample_test 2 2 example_l;
  assert_equal [ [ 3 ]; [ 5 ]; [ 7 ]; [ 9 ] ] @@ fold_sample_test 3 1 example_l;
  assert_equal [ [ 4 ]; [ 6 ]; [ 8 ] ] @@ fold_sample_test 3 2 example_l;
  assert_equal [] @@ fold_sample_test 11 1 example_l;
  assert_equal [] @@ fold_sample_test 12 2 example_l;
  assert_equal [] @@ fold_sample_test 1 1 [];
  assert_equal [] @@ fold_sample_test 1 2 [];
  assert_equal [ [ 1 ] ] @@ fold_sample_test 1 1 [ 1 ];
  assert_equal [] @@ fold_sample_test 1 2 [ 1 ]
let ngram_test = "N Gram" >: test_list [ "ngrams" >:: test_ngrams ]

let test_desome _ = assert_equal 1 @@ desome (Some 1)

let test_invalid_desome _ =
  let invalid_desome _ = desome None in
  assert_raises (Invalid_argument "none case") invalid_desome

let my_test =
  "my_test"
  >: test_list
       [ "desome" >:: test_desome; "invalid_desome" >:: test_invalid_desome ]

module Int_Map = Map.Make (Int)

let ee = list_to_map [ 4; 2; 3; 2; 3; 5; 5; 5; 5; 5; 3; 1; 1; 4 ] Int_Map.empty

let test_list_to_map _ =
  assert_equal [ 1; 2; 3; 4; 5 ] @@ Map.keys ee;
  assert_equal [ 2; 2; 3; 2; 5 ] @@ Map.data ee;
  assert_equal [] @@ Map.keys Int_Map.empty;
  assert_equal [] @@ Map.data Int_Map.empty

module String_list = List_key (String)
module Str_List_Map = Map.Make (String_list)

let a =
  list_to_map
    [
      [ "1" ];
      [ "4" ];
      [ "5" ];
      [ "2" ];
      [ "3" ];
      [ "2" ];
      [ "1" ];
      [ "3" ];
      [ "3" ];
      [ "9"; "9" ];
    ]
    Str_List_Map.empty

let b =
  list_to_map
    [
      [ "3" ];
      [ "3" ];
      [ "3" ];
      [ "2" ];
      [ "2" ];
      [ "2" ];
      [ "1" ];
      [ "1" ];
      [ "1" ];
    ]
    Str_List_Map.empty

(* BASE QUICK CHECK *)
let rand_string =
  let string_gen = String.quickcheck_generator in
  Quickcheck.random_value ~seed:`Nondeterministic string_gen

let p2_test =
  "A4 P2 tests"
  >: test_list
       [
         "list_to_map" >:: test_list_to_map;
         (* "most_frequent_grams" >:: test_most_frequent_grams;
            "rand_num" >:: test_rand_num; *)
       ]

let test_wining_sequence _ =
  assert_equal [ [ 1; 2; 3 ]; [ 3; 4; 5 ]; [ 5; 6; 7 ] ]
  @@ wining_sequence 3 1 [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  assert_equal [ [ 2; 3; 4 ]; [ 4; 5; 6 ]; [ 6; 7; 8 ] ]
  @@ wining_sequence 3 2 [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  assert_equal [ [ 2; 3; 4; 5 ]; [ 4; 5; 6; 7 ] ]
  @@ wining_sequence 4 1 [ 1; 2; 3; 4; 5; 6; 7; 8 ];
  assert_equal [ [ 1; 2; 3; 4 ]; [ 3; 4; 5; 6 ]; [ 5; 6; 7; 8 ] ]
  @@ wining_sequence 4 2 [ 1; 2; 3; 4; 5; 6; 7; 8 ]

let test_pair _ =
  assert_equal 1 @@ pair (1, 2) 1;
  assert_equal 2 @@ pair (1, 2) 2

(*  for coverage purpose*)
let test_random_distribution _ = assert_equal 0 @@ (random_distribution 1 * 0)

let test_player2_first_move _ =
  assert_equal (0, 3) @@ player2_first_move (0, 0);
  assert_equal (0, 2) @@ player2_first_move (0, 1);
  assert_equal (0, 3) @@ player2_first_move (0, 2);
  assert_equal (1, 3) @@ player2_first_move (0, 3);
  assert_equal (0, 3) @@ player2_first_move (0, 4);
  assert_equal (0, 4) @@ player2_first_move (0, 5);
  assert_equal (0, 3) @@ player2_first_move (0, 6)

let test_ai_is_valid_move _ =
  assert_equal false @@ ai_is_valid_move (0, 0) [ (0, 0) ];
  assert_equal true @@ ai_is_valid_move (0, 2) [ (0, 0) ]

let test_get_last_n_moves _ =
  assert_equal [] @@ get_last_n_moves 5 [];
  assert_equal [] @@ get_last_n_moves 5 [ (0, 0) ];
  assert_equal [] @@ get_last_n_moves 5 [ (0, 0); (1, 1) ];
  assert_equal [ (0, 0); (1, 1); (2, 2) ]
  @@ get_last_n_moves 5 [ (0, 0); (1, 1); (2, 2) ];
  assert_equal [ (0, 0); (1, 1); (2, 2); (3, 3) ]
  @@ get_last_n_moves 5 [ (0, 0); (1, 1); (2, 2); (3, 3) ];
  assert_equal [ (0, 0); (1, 1); (2, 2); (3, 3) ]
  @@ get_last_n_moves 4 [ (0, 0); (1, 1); (2, 2); (3, 3) ];
  assert_equal [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4) ]
  @@ get_last_n_moves 5 [ (0, 0); (1, 1); (2, 2); (3, 3); (4, 4) ]

let test_history_to_board _ =
  assert_equal
    [
      [ 1; 0; 0; 1; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
    ]
  @@ history_to_board [ (0, 3); (1, 3); (0, 0) ];
  assert_equal
    [
      [ 1; 1; 1; 1; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 2; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
      [ 0; 0; 0; 0; 0; 0; 0 ];
    ]
  @@ history_to_board
       [ (0, 3); (1, 3); (0, 0); (2, 3); (0, 1); (3, 3); (0, 2) ]
      

let almost_full_h =
  [
    (0, 0);
    (0, 1);
    (0, 2);
    (0, 3);
    (0, 4);
    (0, 5);
    (0, 6);
    (1, 0);
    (1, 1);
    (1, 2);
    (1, 3);
    (1, 4);
    (1, 5);
    (1, 6);
    (2, 0);
    (2, 1);
    (2, 2);
    (2, 3);
    (2, 4);
    (2, 5);
    (2, 6);
    (3, 0);
    (3, 1);
    (3, 2);
    (3, 3);
    (3, 4);
    (3, 5);
    (3, 6);
    (4, 0);
    (4, 1);
    (4, 2);
    (4, 3);
    (4, 4);
    (4, 5);
    (4, 6);
    (5, 0);
    (5, 1);
    (5, 2);
    (5, 3);
    (5, 4);
    (5, 5);
  ]

let full_h = almost_full_h @ [ (5, 6) ]

let test_ai_dist_move _ =
  assert_equal (5, 6) @@ ai_dist_move standard_distribution almost_full_h

module Pos = struct
  type t = int * int [@@deriving compare, sexp]
end

module Pos_grams = N_grams (Pos)

let abc =
  Pos_grams.ngrams 4 1
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]

let bcd =
  Pos_grams.ngrams 4 2
    [ (0, 1); (0, 2); (0, 3); (0, 4); (0, 5); (0, 6); (0, 7) ]

let test_ai_move _ =
  assert_equal (0, 5)
  @@ ai_move [ (0, 2); (0, 3); (0, 4) ] 3 abc standard_distribution 10;
  assert_equal (0, 4)
  @@ ai_move [ (0, 1); (0, 2); (0, 3) ] 3 bcd standard_distribution 10

(* let test_look_around _ =
  assert_equal (5, 6) @@ look_around standard_distribution almost_full_h *)

let new_lib_test =
  "new lib test"
  >: test_list
       [
         (* "Merge Distribution" >:: test_merge_distribution; *)
         (* "Look Around" >:: test_look_around; *)
         "AI Move" >:: test_ai_move;
         "AI Dist Move" >:: test_ai_dist_move;
         "History to Board" >:: test_history_to_board;
         "Winning Sequence" >:: test_wining_sequence;
         "Pair" >:: test_pair;
         "Random Move" >:: test_random_distribution;
         "Player2 First Move" >:: test_player2_first_move;
         "Test Is Valid Move" >:: test_ai_is_valid_move;
         "Test Get Last N Moves" >:: test_get_last_n_moves;
       ]

let gameOverBoard1 =
  [
    [ 0; 1; 1; 1; 1; 0; 0 ];
    [ 0; 2; 2; 2; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let gameOverBoard2 =
  [
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let gameOverBoard3 =
  [
    [ 0; 1; 2; 1; 2; 0; 0 ];
    [ 0; 2; 1; 2; 2; 0; 0 ];
    [ 0; 0; 0; 1; 1; 0; 0 ];
    [ 0; 0; 0; 0; 1; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let gameOverBoard4 =
  [
    [ 0; 1; 2; 2; 2; 2; 0 ];
    [ 0; 1; 1; 0; 0; 0; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_basic _ =
  setPlayer 1;
  assert_equal (true, 1) @@ is_game_over gameOverBoard1 [];
  setPlayer 1;
  assert_equal (true, 1) @@ is_game_over gameOverBoard2 [];
  setPlayer 1;
  assert_equal (true, 1) @@ is_game_over gameOverBoard3 [];
  setPlayer 2;
  assert_equal (true, 2) @@ is_game_over gameOverBoard4 []

(* Actual game played with myself *)
let gameOverBoard5 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 0; 2; 2; 1; 2; 1; 2 ];
    [ 0; 0; 0; 0; 0; 2; 1 ];
    [ 0; 0; 0; 0; 0; 0; 2 ];
  ]

let gameOverBoard6 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 0; 2; 2; 1; 2; 1; 1 ];
    [ 0; 0; 0; 0; 2; 2; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let gameOverBoard7 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 1; 2; 2; 1; 2; 1; 2 ];
    [ 0; 0; 0; 0; 2; 2; 1 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
  ]

let game_over_complex _ =
  setPlayer 2;
  assert_equal (true, 2) @@ is_game_over gameOverBoard5 [];
  setPlayer 1;
  assert_equal (true, 1) @@ is_game_over gameOverBoard6 [];
  setPlayer 1;
  assert_equal (true, 1) @@ is_game_over gameOverBoard7 [];
  setPlayer 1;
  assert_equal (true, -1) @@ is_game_over empty full_h

let moveBoard1 =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
  ]

let moveBoard1' =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 1; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
  ]

let moveBoard1'' =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 1; 2; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
  ]

let moveBoard2 =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 0; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let make_move_invalid _ =
  setPlayer 1;
  assert_equal None @@ makeMove moveBoard1 1;
  setPlayer 1;
  assert_equal None @@ makeMove moveBoard1 2;
  setPlayer 1;
  assert_equal None @@ makeMove moveBoard2 1;
  setPlayer 1;
  assert_equal None @@ makeMove moveBoard2 2;
  setPlayer 1;
  assert_equal None @@ makeMove moveBoard2 4;
  setPlayer 1;
  assert_equal None @@ makeMove moveBoard2 5

let make_move_valid _ =
  setPlayer 1;
  assert_equal (Some moveBoard1') @@ makeMove moveBoard1 3;
  setPlayer 2;
  assert_equal (Some moveBoard1'') @@ makeMove moveBoard1' 4

let changeBoard1 =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 0; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let changeBoard1' =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 1; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let changeBoard1'' =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 1; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
  ]

let change_position _ =
  assert_equal changeBoard1' @@ changePos 0 0 3 3 1 changeBoard1;
  assert_equal changeBoard1'' @@ changePos 0 0 6 5 0 changeBoard1'

let set_player _ =
  setPlayer 1;
  assert_equal !currPlayer @@ 1;
  setPlayer 2;
  assert_equal !currPlayer @@ 2

let board_test =
  "Board tests"
  >: test_list
       [
         "Game over basic" >:: game_over_basic;
         "Game over complex" >:: game_over_complex;
         "Make move invalid" >:: make_move_invalid;
         "Make move valid" >:: make_move_valid;
         "Change position" >:: change_position;
         "Set player" >:: set_player;
       ]

let series =
  "Assignment4 P1 & P2 Tests"
  >::: [
         exercise1_2_test;
         ngram_test;
         my_test;
         p2_test;
         new_lib_test;
         board_test;
       ]

let () = run_test_tt_main series
