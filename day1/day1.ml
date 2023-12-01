let input_file_one = "input1.txt"
let input_file_two = "input2.txt"
let test_input = "test_input.txt"

let number_of_line line =
  let pair = String.fold_left (fun acc c -> 
                                    match c with 
                                    | '0'..'9' -> begin
                                                  match acc with
                                                  | (-1, -1) -> (int_of_string (String.make 1 c), int_of_string (String.make 1 c))
                                                  | (fst, _) -> (fst, int_of_string (String.make 1 c))
                                                  end
                                    | _ -> acc) 
                                  (-1, -1) line in
    match pair with
    | (-1, snd) -> snd
    | (fst, snd) -> fst * 10 + snd
                          
let sum_of_lines lines =
  List.fold_right ( fun line acc -> number_of_line line + acc) lines 0

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

let day1_part1 = sum_of_lines (read_lines input_file_one)

let spelled_numbers = ["one"; "two"; "three"; "four"; "five"; "six"; "seven"; "eight"; "nine"]

let is_partly_number seq = List.exists (String.starts_with ~prefix:seq) spelled_numbers 

let is_number seq = List.exists (fun x -> String.equal seq x) spelled_numbers

let number_of_seq seq = 
  let rec find_number lst x =
    match lst with
    | [] -> raise (Failure "Not Found")
    | h :: t -> if seq = h then 1 else 1 + find_number t x in find_number spelled_numbers seq

let update_pair pair digit =
  match pair with
  | (-1, -1) -> (digit, digit)
  | (fst, _) -> (fst, digit) 

let number_of_line line =
  let helper (pair_acc, acc) c =
    let new_acc = Printf.sprintf "%s%c" acc c in
       if is_partly_number new_acc then
        if is_number new_acc then
          let number = number_of_seq new_acc in 
            (update_pair pair_acc number, "")
        else (pair_acc, new_acc)
       else 
        let rec partly substr = 
          let l = String.length substr in 
          let suffix_str = String.sub substr 1 (l - 1) in
          if is_partly_number suffix_str 
            then (pair_acc, suffix_str)
          else partly suffix_str 
         in partly new_acc in

  let pair = String.fold_left (fun (pair_acc, acc) c ->
                                  match c with
                                  | '0'..'9' -> let digit = int_of_string (String.make 1 c) in 
                                                  ((update_pair pair_acc digit), "")
                                  | c -> helper (pair_acc, acc) c
    ) ((-1, -1), "") line in 
  match pair with
  | ((-1, snd), _) -> snd
  | ((fst, snd), _) -> fst * 10 + snd

  let sum_of_lines lines =
    List.fold_right ( fun line acc -> number_of_line line + acc) lines 0

let day_test = sum_of_lines (read_lines test_input)

let day1_part2 = sum_of_lines (read_lines input_file_two)

let a = List.map number_of_line (read_lines input_file_one)
let test = List.map number_of_line (read_lines test_input)