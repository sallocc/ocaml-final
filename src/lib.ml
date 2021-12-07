open Core

(* OLD LIB functions, IGNORE PLEASE same as in A4 *)
let chunks (n : int) (l : 'a list) : 'a list list =
  if n <= 0 then invalid_arg "invalid input n"
  else
    let rec make_list (n : int) (i : int) (l : 'a list) (new_l : 'a list) :
        'a list =
      if n >= i then make_list (n - 1) i l @@ (List.nth_exn l n :: new_l)
      else new_l
    in
    List.mapi
      ~f:(fun i _ ->
        if i + n <= List.length l then make_list (i + n - 1) i l [] else [])
      l
    |> List.filter ~f:(fun x -> not @@ List.is_empty x)

let split_last (l : 'a list) : 'a * 'a list =
  if List.is_empty l then invalid_arg "empty list"
  else (List.rev l |> List.hd_exn, List.rev l |> List.tl_exn |> List.rev)

module List_key (Elt : Map.Key) : Map.Key with type t = Elt.t list = struct
  type t = Elt.t list [@@deriving compare, sexp]
end

module type Randomness = sig
  val int : int -> int
end

module Randomness = struct
  let int n = Base.Random.int n
  (* let int _ = 0 *)
end

let sample (module R : Randomness) (b : 'a Bag.t) : 'a option =
  let bag_to_list (b : 'a Bag.t) (l : 'a list) : 'a list =
    Bag.fold b ~init:l ~f:(fun acc a -> a :: acc)
  in
  if Bag.length b = 0 then None
  else List.nth (bag_to_list b []) (R.int @@ Bag.length b)

module N_grams (Random : Randomness) (Token : Map.Key) = struct
  module Token_list_map : Map.S with type Key.t = Token.t list =
  Map.Make (struct
    type t = Token.t list [@@deriving compare, sexp]
  end)

  type distribution = Token.t Bag.t Token_list_map.t

  module Token_list = List_key (Token)

  let make_kv (n : int) (l : Token.t list) : (Token.t * Token.t list) list =
    chunks n l |> List.map ~f:(fun x -> split_last x)

  let make_keys (n : int) (l : Token.t list) : Token.t list list =
    make_kv n l |> List.map ~f:(fun (_, k) -> k)

  let map_add (map : Token.t Bag.t Token_list_map.t)
      (key : Token_list_map.Key.t) (data : Token.t Bag.t) :
      Token.t Bag.t Token_list_map.t =
    match Token_list_map.find map key with
    | None -> Token_list_map.add_exn map ~key ~data
    | _ -> map

  let make_bag (key : Token.t list) (value_key : ('a * Token_list.t) list)
      (bag : 'a Bag.t) : unit =
    Bag.clear bag;
    List.fold value_key
      ~f:(fun () (v, k) ->
        if Token_list.compare key k = 0 then Bag.add_unit bag v else ())
      ~init:()

  let make_distribution (key_list : Token_list_map.Key.t list)
      (value_key : (Token.t * Token_list.t) list)
      (map : Token.t Bag.t Token_list_map.t) : Token.t Bag.t Token_list_map.t =
    List.fold key_list
      ~f:(fun acc k ->
        let bag = Bag.create () in
        make_bag k value_key bag;
        map_add acc k bag)
      ~init:map

  let ngrams (n : int) (l : Token.t list) : distribution =
    make_distribution (make_keys n l) (make_kv n l) Token_list_map.empty

  let desome (x : Token.t option) : Token.t =
    match x with Some x -> x | _ -> invalid_arg "none case"

  let initialize_list (initial_ngram : Token.t list) (max_length : int) :
      Token.t list =
    if List.length initial_ngram > max_length then
      List.fold initial_ngram ~init:[] ~f:(fun acc x ->
          if List.length acc < max_length then x :: acc else acc)
    else List.rev initial_ngram

  let first_nth (n : int) (l : Token.t list) : Token.t list =
    let rec make_list (i : int) (sub_list : 'a list) (new_list : Token.t list) :
        Token.t list =
      if i > 0 then
        make_list (i - 1) (List.tl_exn sub_list)
          (List.hd_exn sub_list :: new_list)
      else new_list
    in
    make_list n l []

  let sample_sequence (dist : distribution) ~(max_length : int)
      ~(initial_ngram : Token.t list) : Token.t list =
    let rec loop (output_list : Token.t list) (i : int) : Token.t list =
      if i > 0 && max_length > 0 then
        loop
          (Map.fold dist ~init:output_list ~f:(fun ~key:k ~data:v acc ->
               if
                 Token_list.compare
                   (first_nth
                      (match List.length initial_ngram with
                      | 0 -> List.length acc
                      | x -> x)
                      acc)
                   k
                 = 0
                 && List.length acc < max_length
               then desome (sample (module Random) v) :: acc
               else acc))
          (i - 1)
      else List.rev output_list
    in
    loop
      (initialize_list initial_ngram max_length)
      (max_length - List.length initial_ngram)
end

let sanitize (s : string) : string option =
  if
    String.lowercase s
    |> String.filter ~f:(fun x ->
           (Char.( <= ) x 'z' && Char.( >= ) x 'a')
           || (Char.( <= ) x '9' && Char.( >= ) x '0'))
    |> String.is_empty
  then None
  else
    Some
      (String.lowercase s
      |> String.filter ~f:(fun x ->
             (Char.( <= ) x 'z' && Char.( >= ) x 'a')
             || (Char.( <= ) x '9' && Char.( >= ) x '0')))

let split_space_sanitize (l : string list) : string list =
  List.map l ~f:(fun x ->
      String.split_on_chars ~on:[ ' '; '\t'; '\n' ] x
      |> List.filter ~f:(fun x -> not @@ String.( = ) x ""))
  |> List.concat
  |> List.fold ~init:[] ~f:(fun acc x ->
         match sanitize x with Some a -> a :: acc | None -> acc)
  |> List.rev

let list_to_space_string (l : string list) : string =
  List.fold l ~init:"" ~f:(fun acc x ->
      String.( ^ ) acc (if String.is_empty acc then x else String.( ^ ) " " x))

let given_word (l : string list) : string list =
  l |> List.tl_exn |> List.tl_exn |> List.tl_exn |> List.tl_exn
  |> split_space_sanitize

let rec random_word (i : int) (r : int) (l : string list) (n : int)
    (file_word_list : string list) : string list =
  if i < n - 1 then
    random_word (i + 1) r
      (List.nth_exn file_word_list (r + i) :: l)
      n file_word_list
  else List.rev l |> split_space_sanitize

let rand_num (file_word_list : 'a list) (n : int) : int =
  Randomness.int (List.length file_word_list - n + 1)

let list_to_map (l : 'a list) (map : ('a, int, 'b) Map_intf.Map.t) :
    ('a, int, 'b) Map_intf.Map.t =
  List.fold l ~init:map ~f:(fun acc x ->
      Map.update acc x ~f:(fun v -> match v with Some a -> a + 1 | None -> 1))

let most_frequent_grams (map : ('a, int, 'b) Map_intf.Map.t) (n : int)
    ~(compare : 'a -> 'a -> int) : ('a * int) list =
  let rec list_to_map m kv =
    if List.length kv < n then
      let make_kv =
        Map.fold m ~init:([], 0) ~f:(fun ~key:x ~data:y (a, b) ->
            if y > b || (y = b && compare a x = 1) then (x, y) else (a, b))
      in
      let remove_m = match make_kv with x, _ -> Map.remove m x in
      list_to_map remove_m (make_kv :: kv)
    else kv
  in
  list_to_map map []

(* NEW LIB functions for AI operation CHECK HERE*)
let player1_frist_move = (0, 3)

let player1_second_move pos =
  match pos with
  | 0, 0 -> (1, 3)
  | 0, 1 -> (1, 1)
  | 0, 2 -> (0, 5)
  | 1, 3 -> (2, 3)
  | 0, 4 -> (0, 1)
  | 0, 5 -> (1, 5)
  | 0, 6 -> (1, 3)
  | _ -> invalid_arg "invaid input"

let player2_frist_move pos =
  match pos with
  | 0, 0 -> (0, 3)
  | 0, 1 -> (0, 2)
  | 0, 2 -> (0, 3)
  | 0, 3 -> (0, 3)
  | 0, 4 -> (0, 3)
  | 0, 5 -> (0, 4)
  | 0, 6 -> (0, 3)
  | _ -> invalid_arg "invaid input"

(* 1/7 1/7 1/7 1/7 1/7 1/7 1/7 *)
let random_distribution _ = Random.int 7

let distribution_maker (p0 : int) (p1 : int) (p2 : int) (p3 : int) (p4 : int)
    (p5 : int) (p6 : int) : int =
  if p6 + p5 + p4 + p3 + p1 + p2 + p0 = 100 then
    let r = Random.int 100 in
    if r <= p6 then 6
    else if r <= p6 + p5 then 5
    else if r <= p6 + p5 + p4 then 4
    else if r <= p6 + p5 + p4 + p3 then 3
    else if r <= p6 + p5 + p4 + p3 + p2 then 2
    else if r <= p6 + p5 + p4 + p3 + p1 then 1
    else 0
  else invalid_arg "invalid distribution"

let standard_distribution _ = distribution_maker 9 13 18 20 18 13 9

let left_skewed_distribution _ = distribution_maker 17 20 17 15 13 10 8

let right_skewed_distribution _ = distribution_maker 8 10 13 15 17 20 17

let every_other_chunks (n : int) (odd_even : int) (l : 'a list) : 'a list list =
  chunks n l |> List.filteri ~f:(fun i _ -> i % 2 = odd_even)

let wining_sequence (l : 'a list) (winner : int) (n : int) : 'a list list =
  match (winner, n % 2) with
  | 1, 1 -> every_other_chunks n 0 l
  | 1, 0 -> every_other_chunks n 1 l
  | 2, 1 -> every_other_chunks n 1 l
  | 2, 0 -> every_other_chunks n 0 l
  | _ -> invalid_arg "winner has wrong value"

let pair a n =
  match (a, n) with
  | (x, _), 1 -> x
  | (_, y), 2 -> y
  | _ -> invalid_arg "wrong n"

(* tested correct *)
let is_valid_move (move : int * int) (history : (int * int) list) : bool =
  pair (List.unzip history) 1
  |> List.foldi ~init:true ~f:(fun i acc x ->
         if
           x = pair move 1
           && List.nth_exn (pair (List.unzip history) 2) i = pair move 2
         then acc && false
         else acc && true)

(* for trainning purposes *)
(* let move_given_dist (history : (int * int) list)  ~(dist) =
   if dist _ *)


(* 3 GRAMS: *)
(* IF AI go second *)
(* n chunks if ai wins start from begining sample every other sequence *)
(* if user wins chops out the first move and sample ever other sequences *)

(* IF AI go first *)
(* n chunks if ai wins start from the second move chop off the first, sample every other sequence *)
(* if user wins start sampling from first move and sample ever other sequences *)

(* 4 GRAMS: *)
(* IF AI go first *)
(* n chunks if ai wins start from begining sample every other sequence *)
(* if user wins chops out the first move and sample ever other sequences *)

(* IF AI go second *)
(* n chunks if ai wins start from the second move chop off the first, sample every other sequence *)
(* if user wins start sampling from first move and sample ever other sequences *)
(* let () = Out_channel.write_all "store.txt" ~data:[1;2;3;4] *)
(* let currPlayer = ref 1 *)
(* AI takes in the current game move history to decide which move to make
   if a move is not valid repeat until valid or -1 +1 -2 +2 from the AI's last return.... *)

(*
        0 1 2 3 4 5 6
    5   0 0 0 0 0 0 0
    4   0 0 0 0 0 0 0
    3   0 0 0 0 0 0 0
    2   0 0 0 0 0 0 0
    1   0 0 0 0 0 0 0
    0   0 0 0 0 0 0 0
*)
(*
     let a =[[0;0;0;0;0;0;0];
             [0;0;0;0;0;0;0];
             [0;0;0;0;0;0;0];
             [0;0;0;0;0;0;0];
             [0;0;0;0;0;0;0];
             [0;0;0;0;0;0;0]]
           ;;
*)
(*
     let aa =[[0;0;0;0;0;0;0];
             [0;1;0;0;0;0;0];
             [0;0;1;0;0;0;0];
             [0;0;0;1;0;0;0];
             [0;0;0;0;1;0;0];
             [0;0;0;0;0;0;0]]
           ;;
*)
(*
     let b =[[0;0;0;1;0;0;0];
             [0;1;0;0;0;0;0];
             [1;0;0;1;0;0;0];
             [0;0;1;0;0;0;0];
             [0;1;0;0;0;0;0];
             [1;0;0;0;0;0;0]]
           ;;
*)

(*
     let c =[[1;1;1;1;0;0;0];
             [0;1;0;0;0;0;0];
             [1;0;0;1;0;0;0];
             [0;0;1;0;0;0;0];
             [0;1;0;0;0;0;0];
             [1;0;0;0;0;0;0]]
           ;;
*)

(*
     let d =[[1;0;0;0;0;0;0];
             [0;1;0;1;0;0;0];
             [0;0;0;1;0;0;0];
             [0;0;1;1;0;0;0];
             [0;1;0;1;1;1;1];
             [1;1;1;0;1;1;1]]
           ;;
*)

(*
     let e =[[1;1;1;1;1;0;0];
             [1;0;0;0;0;0;1];
             [1;0;0;1;0;0;0];
             [1;1;0;0;0;1;0];
             [1;1;0;0;1;0;0];
             [1;0;0;1;0;0;0]]
           ;;
*)

(*
     let g =[[1;1;1;1;1;0;0];
             [1;0;1;0;0;0;1];
             [1;0;0;1;0;0;0];
             [1;1;0;0;1;1;0];
             [1;1;0;0;1;1;0];
             [1;0;0;1;0;0;0]]
           ;;
*)