module AoC = struct
  let input_file_of_day day = Printf.sprintf "day%i/input.txt" day
  let test_input_file_of_day day = Printf.sprintf "day%i/test_input.txt" day

  let read_lines file =
    let contents = In_channel.with_open_bin file In_channel.input_all in
    String.split_on_char '\n' contents

  (** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
  let insert k v lst = (k, v) :: lst

  (** [lookup k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
  let rec lookup k = function
    | [] -> None
    | (k', v) :: t -> if k = k' then Some v else lookup k t
end
