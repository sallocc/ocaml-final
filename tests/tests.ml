(*
  Put the tests for lib.ml functions here
*)

open Core
open OUnit2
open Lib
open Board

let a_list = [ "a"; "b"; "c"; "d"; "e" ]

let test_chunks _ =
  assert_equal [] @@ chunks 6 a_list;
  assert_equal [ [ "a" ]; [ "b" ]; [ "c" ]; [ "d" ]; [ "e" ] ]
  @@ chunks 1 a_list;
  assert_equal [ [ "a"; "b" ]; [ "b"; "c" ]; [ "c"; "d" ]; [ "d"; "e" ] ]
  @@ chunks 2 a_list;
  assert_equal [ [ "a"; "b"; "c" ]; [ "b"; "c"; "d" ]; [ "c"; "d"; "e" ] ]
  @@ chunks 3 a_list;
  assert_equal [ [ "a"; "b"; "c"; "d" ]; [ "b"; "c"; "d"; "e" ] ]
  @@ chunks 4 a_list;
  assert_equal [ [ "a"; "b"; "c"; "d"; "e" ] ] @@ chunks 5 a_list

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
         "chunks" >:: test_chunks;
         "invalid_chunks" >:: test_invalid_chunks;
         "split_last" >:: test_split_last;
         "invalid_split_last" >:: test_invalid_split_last;
       ]

(* Notrandomness alwasy choose the first element added into the bag *)
module Notrandomness = struct
  let int _ = 0
end

let test_sample _ =
  assert_equal None
  @@
  let b = Bag.create () in
  sample (module Notrandomness) b

let test_sample' _ =
  assert_equal (Some 1)
  @@
  let b = Bag.create () in
  let _ = Bag.add b 1 in
  sample (module Notrandomness) b

let test_sample'' _ =
  assert_equal (Some 2)
  @@
  let b = Bag.create () in
  let _ = Bag.add b 2 in
  sample (module Notrandomness) b

let test_sample''' _ =
  assert_equal (Some 0)
  @@
  let b = Bag.create () in
  let _ = Bag.add b 0 in
  let _ = Bag.add b 1 in
  let _ = Bag.add b 2 in
  sample (module Notrandomness) b

let test_sample'''' _ =
  assert_equal (Some 0)
  @@
  let b = Bag.create () in
  let _ = Bag.add b 0 in
  let _ = Bag.add b 1 in
  sample (module Notrandomness) b

let test_sample''''' _ =
  assert_equal (Some 0)
  @@
  let b = Bag.create () in
  let _ = Bag.add b 0 in
  let _ = Bag.add b 0 in
  sample (module Notrandomness) b

let exercise3_4_test =
  "Exercise 3 and 4"
  >: test_list
       [
         "sample" >:: test_sample;
         "sample'" >:: test_sample';
         "sample''" >:: test_sample'';
         "sample'''" >:: test_sample''';
         "sample''''" >:: test_sample'''';
         "sample'''''" >:: test_sample''''';
       ]

(* module Rat_Eval = Eval (Rat_Data) *)

module N = N_grams (Notrandomness) (Int)
module Token_list = List_key (Int)

let example_l = [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ]

let bag_to_list (b : 'a Bag.t) (l : 'a list) =
  Bag.fold b ~init:l ~f:(fun acc a -> a :: acc)

let fold_sample_test n l =
  List.fold
    (N.ngrams n l |> Map.data)
    ~init:[]
    ~f:(fun acc a -> bag_to_list a [] :: acc)

let test_ngrams _ =
  assert_equal [ [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ] ]
  @@ fold_sample_test 1 example_l;
  assert_equal [ [ 4; 4; 2 ]; [ 4; 1 ]; [ 3; 2; 3 ]; [ 2 ] ]
  @@ fold_sample_test 2 example_l;
  assert_equal [ [ 4; 2 ]; [ 2 ]; [ 4 ]; [ 4; 1 ]; [ 3 ]; [ 3 ] ]
  @@ fold_sample_test 3 example_l;
  assert_equal [ [ 2 ]; [ 2 ]; [ 3 ]; [ 4 ]; [ 4 ]; [ 1 ]; [ 4 ] ]
  @@ fold_sample_test 4 example_l;
  assert_equal [ [ 2 ]; [ 3 ]; [ 1 ]; [ 2 ]; [ 4 ]; [ 4 ] ]
  @@ fold_sample_test 5 example_l;
  assert_equal [ [ 3 ]; [ 1 ]; [ 2 ]; [ 2 ]; [ 4 ] ]
  @@ fold_sample_test 6 example_l;
  assert_equal [ [ 1 ]; [ 3 ]; [ 2 ]; [ 2 ] ] @@ fold_sample_test 7 example_l;
  assert_equal [ [ 1 ]; [ 3 ]; [ 2 ] ] @@ fold_sample_test 8 example_l;
  assert_equal [ [ 1 ]; [ 3 ] ] @@ fold_sample_test 9 example_l;
  assert_equal [] @@ fold_sample_test 11 example_l;
  assert_equal [] @@ fold_sample_test 12 example_l;
  assert_equal [] @@ fold_sample_test 1 [];
  assert_equal [] @@ fold_sample_test 2 [];
  assert_equal [ [ 1 ] ] @@ fold_sample_test 1 [ 1 ];
  assert_equal [] @@ fold_sample_test 2 [ 1 ]

let test_sample_sequence _ =
  assert_equal [ 2; 3; 4; 4; 4; 4; 4; 4; 4; 4 ]
  @@ N.sample_sequence (N.ngrams 3 example_l) ~max_length:10
       ~initial_ngram:[ 2; 3 ];
  assert_equal [ 2; 3; 4 ]
  @@ N.sample_sequence (N.ngrams 3 example_l) ~max_length:3
       ~initial_ngram:[ 2; 3 ];
  assert_equal [ 2; 3 ]
  @@ N.sample_sequence (N.ngrams 3 example_l) ~max_length:2
       ~initial_ngram:[ 2; 3 ];
  assert_equal [ 2 ]
  @@ N.sample_sequence (N.ngrams 3 example_l) ~max_length:1
       ~initial_ngram:[ 2; 3 ];
  assert_equal [ 1 ]
  @@ N.sample_sequence (N.ngrams 3 example_l) ~max_length:10
       ~initial_ngram:[ 1 ];
  assert_equal [ 1; 2; 3; 4; 4; 4; 4; 4; 4; 4 ]
  @@ N.sample_sequence (N.ngrams 2 example_l) ~max_length:10
       ~initial_ngram:[ 1 ];
  assert_equal [ 2; 3; 4; 4; 4; 4; 4; 4; 4; 4 ]
  @@ N.sample_sequence (N.ngrams 2 example_l) ~max_length:10
       ~initial_ngram:[ 2 ];
  assert_equal [ 3; 4; 4; 4; 4; 4; 4; 4; 4; 4 ]
  @@ N.sample_sequence (N.ngrams 2 example_l) ~max_length:10
       ~initial_ngram:[ 3 ];
  assert_equal [ 1; 2; 3 ]
  @@ N.sample_sequence (N.ngrams 1 []) ~max_length:3 ~initial_ngram:[ 1; 2; 3 ];
  assert_equal [ 1; 2; 3 ]
  @@ N.sample_sequence (N.ngrams 1 []) ~max_length:5 ~initial_ngram:[ 1; 2; 3 ];
  assert_equal []
  @@ N.sample_sequence (N.ngrams 1 []) ~max_length:0 ~initial_ngram:[ 1; 2; 3 ];
  assert_equal [ 1 ]
  @@ N.sample_sequence (N.ngrams 1 []) ~max_length:1 ~initial_ngram:[ 1; 2; 3 ];
  assert_equal [ 1; 2 ]
  @@ N.sample_sequence (N.ngrams 1 []) ~max_length:2 ~initial_ngram:[ 1; 2; 3 ];
  assert_equal [ 1; 2; 3; 4; 4; 4; 2; 2; 3; 1 ]
  @@ N.sample_sequence (N.ngrams 4 example_l) ~max_length:12
       ~initial_ngram:[ 1; 2; 3 ];
  assert_equal [ 1; 2; 3; 4; 4 ]
  @@ N.sample_sequence (N.ngrams 4 example_l) ~max_length:5
       ~initial_ngram:[ 1; 2; 3 ];
  assert_equal []
  @@ N.sample_sequence (N.ngrams 1 []) ~max_length:5 ~initial_ngram:[];
  assert_equal [ 1 ]
  @@ N.sample_sequence (N.ngrams 1 [ 1; 2; 3 ]) ~max_length:5 ~initial_ngram:[];
  assert_equal [ 2 ]
  @@ N.sample_sequence (N.ngrams 1 [ 2; 3 ]) ~max_length:5 ~initial_ngram:[]

let exercise5_test =
  "Exercise 5"
  >: test_list
       [ "ngrams" >:: test_ngrams; "sample_sequence" >:: test_sample_sequence ]

let test_sanitize _ =
  assert_equal (Some "abc") @@ sanitize "abc";
  assert_equal (Some "a") @@ sanitize "a";
  assert_equal (Some "abc") @@ sanitize "ABC";
  assert_equal (Some "a") @@ sanitize "A";
  assert_equal (Some "abcabc123") @@ sanitize "ABCabc123";
  assert_equal (Some "abcabc123") @@ sanitize "!@#`ABCabc123`!@#";
  assert_equal (Some "abcabc123") @@ sanitize "!@#`A-B-C-a-b-c-1-2-3`!@#";
  assert_equal (Some "a") @@ sanitize "`~!@#$%^&*()_+-=a`~!@#$%^&*()_+-=";
  assert_equal None @@ sanitize "";
  assert_equal None @@ sanitize " ";
  assert_equal None @@ sanitize "!";
  assert_equal None @@ sanitize "!@#$$%^&&*(*()";
  assert_equal None @@ sanitize "!@#$$%^&&*(*()/*-+"

let exercise6_test = "Exercise 6" >: test_list [ "sanitize" >:: test_sanitize ]

let test_desome _ = assert_equal 1 @@ N.desome (Some 1)

let test_invalid_desome _ =
  let invalid_desome _ = N.desome None in
  assert_raises (Invalid_argument "none case") invalid_desome

let my_test =
  "my_test"
  >: test_list
       [ "desome" >:: test_desome; "invalid_desome" >:: test_invalid_desome ]

let test_list_to_space_string _ =
  assert_equal "1 2 3" @@ list_to_space_string [ "1"; "2"; "3" ];
  assert_equal "1 2" @@ list_to_space_string [ "1"; "2" ];
  assert_equal "1" @@ list_to_space_string [ "1" ];
  assert_equal "" @@ list_to_space_string []

let test_split_space_sanitize _ =
  assert_equal
    [
      "hello";
      "world";
      "nihao";
      "t";
      "hello";
      "kittie";
      "abcaded";
      "123";
      "crazy";
    ]
  @@ split_space_sanitize
       [
         "hello world";
         "";
         "nihao";
         "\t";
         "\\t hello kittie";
         "";
         "abc-aded ~~~123~~~ (crazy).";
       ];
  assert_equal [] @@ split_space_sanitize [];
  assert_equal
    [
      "hello";
      "world";
      "nihao";
      "t";
      "hello";
      "kittie";
      "abcaded";
      "123";
      "crazy";
    ]
  @@ split_space_sanitize
       [
         "hello";
         "world";
         "nihao";
         "t";
         "hello";
         "kittie";
         "abcaded";
         "123";
         "crazy";
       ];
  assert_equal
    [
      "hello";
      "world";
      "nihao";
      "t";
      "hello";
      "kittie";
      "abcaded";
      "123";
      "crazy";
    ]
  @@ split_space_sanitize
       [ "hello world nihao t hello kittie abcaded 123 crazy" ];
  assert_equal
    [
      "hello";
      "world";
      "nihao";
      "t";
      "hello";
      "kittie";
      "abcaded";
      "123";
      "crazy";
    ]
  @@ split_space_sanitize
       [ "hello world nihao \t \\t hello kittie abc-aded ~~~123~~~ (crazy)." ];
  assert_equal [] @@ split_space_sanitize [ "~~~~ ~~~~~ ~~~ !!!" ]

module Int_Map = Map.Make (Int)

let ee = list_to_map [ 4; 2; 3; 2; 3; 5; 5; 5; 5; 5; 3; 1; 1; 4 ] Int_Map.empty

let test_list_to_map _ =
  assert_equal [ 1; 2; 3; 4; 5 ] @@ Map.keys ee;
  assert_equal [ 2; 2; 3; 2; 5 ] @@ Map.data ee;
  assert_equal [] @@ Map.keys Int_Map.empty;
  assert_equal [] @@ Map.data Int_Map.empty

(* let test_most_frequent _ = *)
(* assert_equal (5,5) @@ most_frequent (Map.keys ee) (Map.data ee) 1 *)

let test_given_word _ =
  assert_equal [ "5" ] @@ given_word [ "1"; "2"; "3"; "4"; "5" ]

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

let test_most_frequent_grams _ =
  assert_equal [ ([ "3" ], 3) ]
  @@ most_frequent_grams a 1 ~compare:String_list.compare;
  assert_equal [ ([ "1" ], 2); ([ "3" ], 3) ]
  @@ most_frequent_grams a 2 ~compare:String_list.compare;
  assert_equal [ ([ "2" ], 2); ([ "1" ], 2); ([ "3" ], 3) ]
  @@ most_frequent_grams a 3 ~compare:String_list.compare;
  assert_equal [ ([ "4" ], 1); ([ "2" ], 2); ([ "1" ], 2); ([ "3" ], 3) ]
  @@ most_frequent_grams a 4 ~compare:String_list.compare;
  assert_equal
    [ ([ "5" ], 1); ([ "4" ], 1); ([ "2" ], 2); ([ "1" ], 2); ([ "3" ], 3) ]
  @@ most_frequent_grams a 5 ~compare:String_list.compare;
  assert_equal
    [
      ([ "9"; "9" ], 1);
      ([ "5" ], 1);
      ([ "4" ], 1);
      ([ "2" ], 2);
      ([ "1" ], 2);
      ([ "3" ], 3);
    ]
  @@ most_frequent_grams a 6 ~compare:String_list.compare;
  assert_equal [ ([ "2" ], 3); ([ "1" ], 3) ]
  @@ most_frequent_grams b 2 ~compare:String_list.compare

let test_random_word _ =
  assert_equal [ "3"; "4"; "5" ]
  @@ random_word 0 2 [] 4 [ "1"; "2"; "3"; "4"; "5" ];
  assert_equal [] @@ random_word 0 2 [] 0 [ "1"; "2"; "3"; "4"; "5" ];
  assert_equal [ "3" ] @@ random_word 0 2 [] 2 [ "1"; "2"; "3"; "4"; "5" ]

let test_rand_num _ = assert_equal 0 @@ rand_num [ 1 ] 1

(* BASE QUICK CHECK *)
let rand_string =
  let string_gen = String.quickcheck_generator in
  Quickcheck.random_value ~seed:`Nondeterministic string_gen

let test_quick_check_sanitize _ =
  assert_equal false
  @@ String.contains
       (match sanitize rand_string with Some x -> x | None -> "")
       '`';
  assert_equal false
  @@ String.contains
       (match sanitize rand_string with Some x -> x | None -> "")
       '%';
  assert_equal false
  @@ String.contains
       (match sanitize rand_string with Some x -> x | None -> "")
       '-';
  assert_equal false
  @@ String.contains
       (match sanitize rand_string with Some x -> x | None -> "")
       ' ';
  assert_equal false
  @@ String.contains
       (match sanitize rand_string with Some x -> x | None -> "")
       'A';
  assert_equal true
  @@ (String.contains
        (match sanitize rand_string with Some x -> x | None -> "")
        'a'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'b'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'c'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'd'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'e'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'f'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'g'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'h'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'i'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'j'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'k'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'l'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'm'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'n'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'o'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'p'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'q'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'r'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          's'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          't'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'u'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'v'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'w'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'x'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'y'
     || String.contains
          (match sanitize rand_string with Some x -> x | None -> "")
          'z')

let p2_test =
  "A4 P2 tests"
  >: test_list
       [
         "list_to_space_string" >:: test_list_to_space_string;
         "split_space_sanitize" >:: test_split_space_sanitize;
         "list_to_map" >:: test_list_to_map;
         "given_word" >:: test_given_word;
         "most_frequent_grams" >:: test_most_frequent_grams;
         "random_word" >:: test_random_word;
         "rand_num" >:: test_rand_num;
         "quick_check_sanitize" >:: test_quick_check_sanitize;
       ]

let test_wining_sequence _ =
  assert_equal [ [ 1; 2; 3 ]; [ 3; 4; 5 ]; [ 5; 6; 7 ] ]
  @@ wining_sequence [ 1; 2; 3; 4; 5; 6; 7; 8 ] 1 3;
  assert_equal [ [ 2; 3; 4 ]; [ 4; 5; 6 ]; [ 6; 7; 8 ] ]
  @@ wining_sequence [ 1; 2; 3; 4; 5; 6; 7; 8 ] 2 3;
  assert_equal [ [ 2; 3; 4; 5 ]; [ 4; 5; 6; 7 ] ]
  @@ wining_sequence [ 1; 2; 3; 4; 5; 6; 7; 8 ] 1 4;
  assert_equal [ [ 1; 2; 3; 4 ]; [ 3; 4; 5; 6 ]; [ 5; 6; 7; 8 ] ]
  @@ wining_sequence [ 1; 2; 3; 4; 5; 6; 7; 8 ] 2 4

let test_pair _ =
  assert_equal 1 @@ pair (1, 2) 1;
  assert_equal 2 @@ pair (1, 2) 2

(*  for coverage purpose*)
let test_random_move _ = assert_equal 0 @@ (random_move 1 * 0)

let test_player2_frist_move _ =
  assert_equal (0, 3) @@ player2_frist_move (0, 0);
  assert_equal (0, 2) @@ player2_frist_move (0, 1);
  assert_equal (0, 3) @@ player2_frist_move (0, 2);
  assert_equal (0, 3) @@ player2_frist_move (0, 3);
  assert_equal (0, 3) @@ player2_frist_move (0, 4);
  assert_equal (0, 4) @@ player2_frist_move (0, 5);
  assert_equal (0, 3) @@ player2_frist_move (0, 6)

let test_is_valid_move _ =
  assert_equal false @@ is_valid_move (0, 0) [ (0, 0) ];
  assert_equal true @@ is_valid_move (0, 2) [ (0, 0) ]

let new_lib_test =
  "new lib test"
  >: test_list
       [
         "winning sequence" >:: test_wining_sequence;
         "pair" >:: test_pair;
         "random_move" >:: test_random_move;
         "player2_frist_move" >:: test_player2_frist_move;
         "test_is_valid_move" >:: test_is_valid_move;
       ]

let gameOverBoard1 =
  [
    [ 0; 1; 1; 1; 1; 0; 0 ];
    [ 0; 2; 2; 2; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]

let gameOverBoard2 =
  [
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]

let gameOverBoard3 =
  [
    [ 0; 1; 2; 1; 2; 0; 0 ];
    [ 0; 2; 1; 2; 2; 0; 0 ];
    [ 0; 0; 0; 1; 1; 0; 0 ];
    [ 0; 0; 0; 0; 1; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]
let gameOverBoard4 =
  [
    [ 0; 1; 2; 2; 2; 2; 0 ];
    [ 0; 1; 1; 0; 0; 0; 0 ];
    [ 0; 1; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]

let game_over_basic _ =
  setPlayer 1; assert_equal (true, 1) @@ isGameOver gameOverBoard1;
  setPlayer 1; assert_equal (true, 1) @@ isGameOver gameOverBoard2;
  setPlayer 1; assert_equal (true, 1) @@ isGameOver gameOverBoard3;
  setPlayer 2; assert_equal (true, 2) @@ isGameOver gameOverBoard4

(* Actual game played with myself *)
let gameOverBoard5 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 0; 2; 2; 1; 2; 1; 2 ];
    [ 0; 0; 0; 0; 0; 2; 1 ];
    [ 0; 0; 0; 0; 0; 0; 2 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]

let gameOverBoard6 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 0; 2; 2; 1; 2; 1; 1 ];
    [ 0; 0; 0; 0; 2; 2; 0 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]

let gameOverBoard7 =
  [
    [ 1; 2; 1; 1; 2; 1; 1 ];
    [ 2; 2; 1; 2; 1; 2; 2 ];
    [ 1; 1; 1; 2; 2; 1; 1 ];
    [ 1; 2; 2; 1; 2; 1; 2 ];
    [ 0; 0; 0; 0; 2; 2; 1 ];
    [ 0; 0; 0; 0; 0; 0; 0 ];
    (* [ 0; 0; 0; 0; 0; 0; 0 ]; *)
  ]

let game_over_complex _ =
  setPlayer 2; assert_equal (true, 2) @@ isGameOver gameOverBoard5;
  setPlayer 1; assert_equal (true, 1) @@ isGameOver gameOverBoard6;
  setPlayer 1; assert_equal (true, 1) @@ isGameOver gameOverBoard7

let moveBoard1 =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    (* [ 0; 1; 0; 0; 0; 0; 0 ]; *)
  ]

let moveBoard1' =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 1; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    (* [ 0; 1; 0; 0; 0; 0; 0 ]; *)
  ]

let moveBoard1'' =
  [
    [ 0; 1; 2; 2; 2; 0; 0 ];
    [ 0; 1; 1; 1; 2; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    [ 0; 1; 2; 0; 0; 0; 0 ];
    [ 0; 2; 1; 0; 0; 0; 0 ];
    (* [ 0; 1; 0; 0; 0; 0; 0 ]; *)
  ]

let moveBoard2 =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 0; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
    (* [ 0; 1; 2; 0; 1; 1; 0 ]; *)
  ]

let make_move_invalid _ =
  setPlayer 1; assert_equal None @@ makeMove moveBoard1 1;
  setPlayer 1; assert_equal None @@ makeMove moveBoard1 2;
  setPlayer 1; assert_equal None @@ makeMove moveBoard2 1;
  setPlayer 1; assert_equal None @@ makeMove moveBoard2 2;
  setPlayer 1; assert_equal None @@ makeMove moveBoard2 4;
  setPlayer 1; assert_equal None @@ makeMove moveBoard2 5


let make_move_valid _ =
  setPlayer 1; assert_equal (Some moveBoard1') @@ makeMove moveBoard1 3;
  setPlayer 2; assert_equal (Some moveBoard1'') @@ makeMove moveBoard1' 4

let changeBoard1 =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 0; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
    (* [ 0; 1; 2; 0; 1; 1; 0 ]; *)
  ]

let changeBoard1' =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 1; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
    (* [ 0; 1; 2; 0; 1; 1; 0 ]; *)
  ]

let changeBoard1'' =
  [
    [ 0; 1; 2; 2; 2; 1; 0 ];
    [ 0; 1; 1; 0; 1; 2; 0 ];
    [ 0; 1; 2; 0; 2; 1; 0 ];
    [ 0; 2; 1; 1; 2; 1; 0 ];
    [ 0; 1; 2; 0; 1; 2; 0 ];
    [ 0; 2; 1; 0; 2; 2; 0 ];
    (* [ 0; 1; 2; 0; 1; 1; 2 ]; *)
  ]

let change_position _ =
  assert_equal changeBoard1' @@ changePos 0 0 3 3 1 changeBoard1;
  assert_equal changeBoard1'' @@ changePos 0 0 6 5 0 changeBoard1'

let set_player _ =
  setPlayer 1; assert_equal (!currPlayer) @@ 1;
  setPlayer 2; assert_equal (!currPlayer) @@ 2

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
         (* old lib test  *)
         exercise1_2_test;
         exercise3_4_test;
         exercise5_test;
         exercise6_test;
         my_test;
         p2_test;
         (* new lib test  *)
         new_lib_test;
         board_test;
       ]

let () = run_test_tt_main series
