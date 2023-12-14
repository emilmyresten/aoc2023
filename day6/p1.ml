let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines =
  Printf.printf "%s\n" (String.concat " " (List.map string_of_int lines))

(* https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
let rec pow x n =
  if n < 0 then pow (1 / x) (-n)
  else if n = 0 then 1
  else if n mod 2 = 0 then pow (x * x) (n / 2)
  else x * pow (x * x) ((n - 1) / 2)

let split_and_trim line =
  line |> String.split_on_char ' '
  |> List.filter (fun s -> String.empty <> s)
  |> List.tl |> List.map String.trim |> List.map int_of_string

let find_winning_holds time_to_beat distance_to_beat =
  let hold_times = List.init time_to_beat succ in
  let time_to_move =
    List.map (fun hold_time -> time_to_beat - hold_time) hold_times
  in
  let distance_traveled =
    List.map2 (fun a b -> a * b) hold_times time_to_move
  in
  List.filter (fun distance -> distance > distance_to_beat) distance_traveled

let () =
  let lines = read_lines (open_in "input.txt") in
  let times_to_beat = split_and_trim (List.hd lines) in
  let distances_to_beat = split_and_trim (List.hd (List.tl lines)) in
  let times_per_distance = List.combine times_to_beat distances_to_beat in
  let winning_holds =
    List.map
      (fun (time, distance) -> find_winning_holds time distance)
      times_per_distance
  in
  List.map List.length winning_holds |> List.fold_left ( * ) 1 |> print_int

(* 32076 *)
