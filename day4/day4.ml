let input_file = "input.txt"
let test_input_file = "test_input.txt"

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

let numbers_of_card card =
  match String.split_on_char ':' card with
  | [_; numbers] -> numbers
  | _ -> ""

let name_and_numbers_of_card card =
  match String.split_on_char ':' card with
  | [name; numbers] -> (name, numbers)
  | _ -> ("", "")

let list_of_numbers = String.split_on_char ' '

let remove_empty = List.fold_left (fun acc x -> if x = "" then acc else x :: acc) []

let split_numbers numbers =
  match String.split_on_char '|' numbers with
  | [wn; my_nums] -> (remove_empty (list_of_numbers wn), remove_empty (list_of_numbers my_nums))
  | _ -> ([], [])

let check_number num = List.exists (fun elt-> elt = num) 

let get_score wn = List.fold_left (fun acc elt -> if check_number elt wn 
                                                  then if acc = 0 then 1 else acc * 2
                                                  else acc) 0

let fold_line_helper acc line = 
  let (wn, my_nums) = line |> numbers_of_card |> split_numbers in
    acc + get_score wn my_nums

let test = List.fold_left fold_line_helper 0 (read_lines test_input_file)
let part_one = List.fold_left fold_line_helper 0 (read_lines input_file)

let get_matching_count wn = List.fold_left (fun acc elt -> if check_number elt wn 
                                                        then acc + 1 else acc) 0

let fold_line_snd_helper acc line = 
  let nums = numbers_of_card line in
  let (wn, my_nums) = split_numbers nums in (1, get_matching_count wn my_nums) :: acc

let assoc_map_test = List.fold_left fold_line_snd_helper [] (read_lines test_input_file)
let assoc_map = List.fold_left fold_line_snd_helper [] (read_lines input_file)

let test_stack = Stack.create ()
let my_stack  = Stack.create ()
let _ = List.fold_left (fun acc elt -> let _ = Stack.push elt acc in acc) test_stack assoc_map_test
let _ = List.fold_left (fun acc elt -> let _ = Stack.push elt acc in acc) my_stack assoc_map

let pop_n n st =
  let popped = Stack.create () in
  let rec pops t =
    match t with
    | 0 -> ()
    | _ -> let x = Stack.pop st in let _ = Stack.push x popped in pops (t - 1) in
  let _ = pops n in popped


let update_stack acc st =
  match Stack.pop_opt st with
  | Some (count, matches) -> let popped = pop_n matches st in 
                           let _ = Stack.fold (fun _ elt -> let (c, m) = elt in Stack.push (c+count, m) st) () popped in
                           (acc + count)
  | None -> acc

let test_length = Stack.length test_stack
let input_length = Stack.length my_stack

let rec rec_update len acc st =
  match len with
  | 0 -> acc
  | _ -> let new_acc = update_stack acc st in rec_update (len - 1) new_acc st

let test_scratch_cards = rec_update test_length 0 test_stack
let won_scratch_cards = rec_update input_length 0 my_stack

