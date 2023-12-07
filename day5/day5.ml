let input_file = "input.txt"
let test_input_file = "test_input.txt"

let read_lines file = 
  let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

List.fold_right (fun x (acc, list_acc) -> if x = "" then ("", acc :: list_acc) else (x ^ "/n" ^ acc, list_acc) ) read_lines ("", [])

