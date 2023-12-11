open Aoc_helpers

exception Not_Key

let instructions_and_dict_of_lines lines =
  match lines with inst :: _ :: dict -> (inst, dict) | _ -> ("", [])

let tuple_of_pair pair =
  let trimed_pair = pair |> String.trim in
  let inside_pair = String.sub trimed_pair 1 8 in
  match String.split_on_char ',' inside_pair with
  | [ left; right ] -> (left, String.trim right)
  | _ -> ("", "")

let keyvalue_of_line line =
  match String.split_on_char '=' line with
  | [ key; pair ] -> (String.trim key, tuple_of_pair pair)
  | _ -> ("", ("", ""))

let dict_of_lines =
  List.fold_left
    (fun acc elt ->
      let k, v = elt |> keyvalue_of_line in
      AoC.insert k v acc)
    []

let follow_inst inst dict start =
  let do_step (start_point, steps) direction =
    if String.sub start_point 2 1 = "Z" then (start_point, steps)
    else
      match AoC.lookup start_point dict with
      | Some pair ->
          let next = if direction = 'L' then pair |> fst else pair |> snd in
          (next, steps + 1)
      | None ->
          Printf.printf "start_point:%s \n" start_point;
          raise Not_Key
  in
  let rec do_steps inst_lst (starting_point, steps) =
    match String.fold_left do_step (starting_point, steps) inst_lst with
    | x, result when String.sub x 2 1 = "Z" -> result
    | pair -> do_steps inst_lst pair
  in
  do_steps inst (start, 0)

let part_one file =
  let i, d = file |> AoC.read_lines |> instructions_and_dict_of_lines in
  let assoc_list = dict_of_lines d in
  follow_inst i assoc_list "AAA"

let rec gcd a b = if b = 0 then a else gcd b (a mod b)
let is_start key = String.sub key 2 1 = "A"

let ghost_starts =
  List.fold_left
    (fun acc elt ->
      let key = fst elt in
      if is_start key then key :: acc else acc)
    []

let list_gcd lst = List.fold_left gcd (List.hd lst) lst

let part_two file =
  let i, d = file |> AoC.read_lines |> instructions_and_dict_of_lines in
  let assoc_list = dict_of_lines d in
  let get_steps_from_start = follow_inst i assoc_list in
  let results = assoc_list |> ghost_starts |> List.map get_steps_from_start in
  let common_denominator = list_gcd results in
  common_denominator
  * List.fold_left (fun acc elt -> acc * elt / common_denominator) 1 results

let () =
  let test_part_one = part_one (AoC.test_input_file_of_day 8) in
  Printf.printf "Test part one: %i \n" test_part_one;
  let part_one_answer = part_one (AoC.input_file_of_day 8) in
  Printf.printf "Part one: %i \n" part_one_answer;
  let test_part_two = part_two "day8/test_input_part_two.txt" in
  Printf.printf "Test part two: %i \n" test_part_two;
  let part_two_answer = part_two (AoC.input_file_of_day 8) in
  Printf.printf "Part two: %i \n" part_two_answer
