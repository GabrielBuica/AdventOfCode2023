let input_file = "input.txt"
let test_input_file = "test_input.txt"

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

(* "32T3K 765" ===> (['3';'2';'T';'3';'K'], 765*)
let hand_and_bid_of_line line =
  match String.split_on_char ' ' line with
  | [hand; bid] -> (List.init 5 (String.get hand), int_of_string bid)
  | _ -> ([], 0)


(** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let get_head lst = 
  match lst with
  | [] -> ' '
  | h :: _ -> h

(* ['3';'2';'T';'3';'K'] ===> ['2'; '3'; 'T']*)
let compress_hand hand = 
  hand
  |> List.sort compare
  |> List.fold_left (fun acc x -> let head = get_head acc in if x = head then acc else x :: acc) []

let ( >> ) f g x = g f x