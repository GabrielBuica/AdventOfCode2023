let input_file = "input.txt"
let test_input = "test_input.txt"
let test_snd_input = "test_input_part_two.txt"

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

let digit_of_char c = 
  match c with
  | '1'..'9' -> Some (c |> String.make 1 |> int_of_string)
  | _ -> None
  
let digit_filter c = 
  match c with
  | '1'..'9' -> true
  | _ -> false

let rev_digit_list_of_string s =
  String.fold_left (fun acc x -> if digit_filter x then (digit_of_char x) :: acc else acc) [] s
  
let get_digits lst =
  let rec get_digits_tr acc lst = 
    match lst with
    | []  -> (-1, -1)
    | [Some x] -> if (snd acc) = -1 then (x, x) else (x, snd acc) 
    | Some h :: t -> if (snd acc) = -1 then get_digits_tr (fst acc, h) t else get_digits_tr acc t 
    | _ ->  (-1, -1) in
    get_digits_tr (-1, -1) lst


let number_of_pair (st, nd) = let x = st * 10 + nd in x 

let get_number_and_new_cursor s =
  let open String in
  match get s 0 with
  | '1'..'9' as c -> (digit_of_char c, 1) 
  | 'o' -> if starts_with ~prefix:"one" s then (Some 1, 2) else (None, 1)
  | 't' -> if starts_with ~prefix:"two" s then (Some 2, 2) 
            else if starts_with ~prefix:"three" s then (Some 3, 5) 
            else (None, 1)
  | 'f' -> if starts_with ~prefix:"four" s then (Some 4, 4) 
            else if starts_with ~prefix:"five" s then (Some 5, 3) 
            else (None, 1)
  | 's' -> if starts_with ~prefix:"six" s then (Some 6, 3) 
            else if starts_with ~prefix:"seven" s then (Some 7, 4) 
            else (None, 1)
  | 'e' -> if starts_with ~prefix:"eight" s then (Some 8, 4) else (None, 1)
  | 'n' -> if starts_with ~prefix:"nine" s then (Some 9, 3) else (None, 1)
  | exception Invalid_argument _ -> (None, 0)
  | _ -> (None, 1)

let rev_digits s = 
  let rec rev_digit_list_of_string' acc s =
    let l = String.length s in 
    match get_number_and_new_cursor s with
    | (None, 0) -> acc
    | (None, 1) -> rev_digit_list_of_string' acc (String.sub s 1 (l-1))
    | (None, _) -> []
    | (Some x, cursor) -> rev_digit_list_of_string' (Some x::acc) (String.sub s cursor (l-cursor)) in
    rev_digit_list_of_string' [] s

let part_one file =
  file 
  |> read_lines
  |> List.map rev_digit_list_of_string
  |> List.map get_digits
  |> List.map number_of_pair
  |> List.fold_left ( + ) 0

let part_two file =
  file
  |> read_lines
  |> List.map rev_digits
  |> List.map get_digits
  |> List.map number_of_pair
  |> List.fold_left ( + ) 0

let () =
  let test_part_one = part_one test_input in
  Printf.printf "Test part one: %i \n" test_part_one;
  let part_one = part_one input_file in
  Printf.printf "Part one: %i \n" part_one;
  let test_part_two = part_two test_snd_input in
  Printf.printf "Test part two: %i \n" test_part_two;
  let part_two = part_two input_file in
  Printf.printf "Part two: %i \n" part_two;

