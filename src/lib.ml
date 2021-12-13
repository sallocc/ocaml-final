open Core

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

let every_other_chunks (n : int) (odd_even : int) (l : 'a list) : 'a list list =
  chunks n l |> List.filteri ~f:(fun i _ -> i % 2 = odd_even)

let wining_sequence (n : int) (winner : int) (l : 'a list) : 'a list list =
  match (winner, n % 2) with
  | 1, 1 -> every_other_chunks n 0 l
  | 1, 0 -> every_other_chunks n 1 l
  | 2, 1 -> every_other_chunks n 1 l
  | 2, 0 -> every_other_chunks n 0 l
  | _ -> invalid_arg "winner has wrong value"

let split_last (l : 'a list) : 'a * 'a list =
  if List.is_empty l then invalid_arg "empty list"
  else (List.rev l |> List.hd_exn, List.rev l |> List.tl_exn |> List.rev)

module List_key (Elt : Map.Key) : Map.Key with type t = Elt.t list = struct
  type t = Elt.t list [@@deriving compare, sexp]
end

let sample (b : 'a Bag.t) : 'a option =
  let bag_to_list (b : 'a Bag.t) (l : 'a list) : 'a list =
    Bag.fold b ~init:l ~f:(fun acc a -> a :: acc)
  in
  if Bag.length b = 0 then None
  else List.nth (bag_to_list b []) (Random.int @@ Bag.length b)

module N_grams (Token : Map.Key) = struct
  module Token_list_map : Map.S with type Key.t = Token.t list =
  Map.Make (struct
    type t = Token.t list [@@deriving compare, sexp]
  end)
  type distribution = Token.t Bag.t Token_list_map.t [@@deriving sexp]
  module Token_list = List_key (Token)

  let make_kv (n : int) (player:int) (l : Token.t list) : (Token.t * Token.t list) list =
    wining_sequence n player l |> List.map ~f:(fun x -> split_last x)

  let make_keys (n : int) (player:int) (l : Token.t list) : Token.t list list =
    make_kv n player l |> List.map ~f:(fun (_, k) -> k)

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

  let ngrams (n : int) (player:int) (l : Token.t list) : distribution =
    make_distribution (make_keys n player l) (make_kv n player l) Token_list_map.empty
  let desome (x : Token.t option) : Token.t =
    match x with Some x -> x | _ -> invalid_arg "none case"
end

let rand_num (file_word_list : 'a list) (n : int) : int =
  Random.int (List.length file_word_list - n + 1)

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

let rec get_n_items (n:int) (original:(int*int) list) (return_list:(int*int) list) : (int*int)list = 
  match n with 
  | 0 -> return_list
  | _ -> get_n_items (n-1) (original) (List.nth_exn original (n-1) :: return_list)

(* n here is n-1 in n_grams *)
let get_last_n_moves (n:int) (history : (int * int) list) : (int * int) list =
  if n < 3 then invalid_arg "n is designed to be greater than 3"
  else if n > 6 then invalid_arg "n is designed to be less than 7"
  else if (List.length history) < 3 then [] 
  else if (List.length history) < n then history
  else get_n_items n history []

(* execution *)

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