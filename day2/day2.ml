let input_file = "input.txt"
let test_input_file = "test_input.txt"

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

let rounds_of_game game =
  match String.split_on_char ':' game with
  | [game; rounds] -> (game, String.trim rounds)
  | _ -> ("", "")

let id_of_game game =
  match String.split_on_char ' ' game with
  | [_; x] -> int_of_string x
  | _ -> 0

let rounds_of_string str = str |> String.split_on_char ';' |> (List.map String.trim)

let max_bag = (12, 13, 14)

let show_triple show = 
  show |> String.split_on_char ',' |> (List.map String.trim)
   
let triple_of_show show = 
  match String.split_on_char ' ' show with
  | [x; "red"] -> (int_of_string x, 0, 0)
  | [x; "green"] -> (0, int_of_string x, 0)
  | [x; "blue"] -> (0, 0, int_of_string x)
  | _ -> (0, 0, 0)

let ( ++ ) x y = 
  let (x1, x2, x3) = x in
  let (y1, y2, y3) = y in (x1 + y1, x2 + y2, x3 + y3)

let ( >> ) x y = 
  let (x1, x2, x3) = x in
  let (y1, y2, y3) = y in x1 > y1 || x2 > y2 || x3 > y3

let impossible x = x >> max_bag 

let triples_of_game game =
  let (g, rounds)  = rounds_of_game game in
  let id = g |> id_of_game in
  let triples =
  rounds
  |> rounds_of_string
  |> (List.map show_triple) 
  |> (List.map (List.map triple_of_show)) 
  |> (List.map (List.fold_left ( ++ ) (0, 0, 0))) in
  let imp = List.exists impossible triples in
  (id, imp)

let max_triple x y = 
  let (x1, x2, x3) = x in
  let (y1, y2, y3) = y in
  let z1 = max x1 y1 in 
  let z2 = max x2 y2 in
  let z3 = max x3 y3 in (z1, z2, z3)

let triple_of_game_part_two game =
  let (_, rounds)  = rounds_of_game game in
  rounds
  |> rounds_of_string
  |> (List.map show_triple) 
  |> (List.map (List.map triple_of_show)) 
  |> (List.map (List.fold_left ( ++ ) (0, 0, 0)))
  |> (List.fold_left max_triple (0, 0, 0))

let sum_of_lines lines =
  List.fold_left (fun acc (id, imp) -> if imp then acc else acc + id) 0 lines
  
let test = sum_of_lines (List.map triples_of_game (read_lines test_input_file))
let part_one = sum_of_lines (List.map triples_of_game (read_lines input_file))
let test_two = let lst = List.map triple_of_game_part_two (read_lines test_input_file) in
                List.fold_left (fun acc (x, y, z) -> acc + x * y * z) 0 lst
let part_two = let lst = List.map triple_of_game_part_two (read_lines input_file) in
                List.fold_left (fun acc (x, y, z) -> acc + x * y * z) 0 lst
