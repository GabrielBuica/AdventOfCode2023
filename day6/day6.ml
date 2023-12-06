let input_file = "input.txt"
let test_input = "test_input.txt"

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

(* " 7  15 30" ===> ["7"; "15"; "30"]*)
let data_list_of_string s =
  let lst = String.split_on_char ' ' s in
  lst 
  |> List.filter (fun x -> x <> "")

(* "Time:      7  15 30" ===> [7; 15; 30]*)
let data_list_of_row s = 
  match String.split_on_char ':' s with
  | [_; data_string] -> data_list_of_string data_string |> List.map float_of_string
  | _ -> []

(* "Time:      7  15 30" ===> 71530*)
let big_number_of_row s =
  match String.split_on_char ':' s with
  | [_; data_string] -> data_list_of_string data_string |> List.fold_left ( ^ ) "" |> float_of_string
    | _ -> 0.

let times_and_distances_of_file file =
  let lines = read_lines file in
  match lines with
  | [t; d] -> (data_list_of_row t, data_list_of_row d)
  | _ -> ([], [])

let big_numbers_of_file file = 
  let lines = read_lines file in
  match lines with
  | [t; d] -> (big_number_of_row t, big_number_of_row d)
  | _ -> (0., 0.)

let get_minmax_time t d = 
  let delta = Float.sqrt (t ** 2. -. 4. *. d) in
  let mint = Float.floor ((t -. delta) /. 2.) in 
  let maxt = Float.ceil ((t +. delta) /. 2.) in 
  (mint +. 1., maxt -. 1.)

let reduce2 f (lst1, lst2) = List.map2 f lst1 lst2

let winning_range t1 (m1, m2) = (min t1 m2) -. m1 +. 1.

let solve_part_one file =
  let (t, d) = file |> times_and_distances_of_file in 
  (t, d)
  |> reduce2 get_minmax_time 
  |> List.map2 winning_range t 
  |> List.fold_left ( *. ) 1.

let solve_part_two file = 
  let (t, d) = file |> big_numbers_of_file in
   get_minmax_time t d |> winning_range t
  
let () =
  let test_part_one = solve_part_one test_input in
  Printf.printf "Test part one: %i \n" (int_of_float test_part_one);
  let part_one = solve_part_one input_file in
  Printf.printf "Part one: %i \n" (int_of_float part_one);
  let test_part_two = solve_part_two test_input in
  Printf.printf "Test part two: %i \n" (int_of_float test_part_two);
  let part_two = solve_part_two input_file in
  Printf.printf "Part two: %i \n" (int_of_float part_two);