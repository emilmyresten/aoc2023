let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines = Printf.printf "%s" (String.concat "\n" lines)

let get_differences seq =
  List.fold_left
    (fun (prev, differences) current_value ->
      match prev with
      | None -> (Some current_value, differences)
      | Some previous_value ->
          let difference = current_value - previous_value in
          (Some current_value, difference :: differences))
    (None, []) seq
  |> snd |> List.rev

let rec next_in_sequence seq =
  (* List.iter (fun m -> Printf.printf "%d " m) seq;
     print_char '\n'; *)
  if List.for_all (fun diff -> diff = 0) seq then 0
  else
    let differences = get_differences seq in
    let first = List.hd seq in
    let next_num = first - next_in_sequence differences in
    next_num

let () =
  let lines =
    read_lines (open_in "input.txt")
    |> List.map (fun line ->
           String.split_on_char ' ' line
           |> List.map String.trim |> List.map int_of_string)
  in
  List.map next_in_sequence lines
  |> List.fold_left ( + ) 0 |> Printf.printf "%d"

(* 1022 *)
