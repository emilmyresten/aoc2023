let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines = Printf.printf "%s" (String.concat "\n" lines)

let winning_numbers numbers =
  let winning, ours = (List.nth numbers 0, List.nth numbers 1) in
  let winning = String.split_on_char ' ' winning
  and ours = String.split_on_char ' ' ours in
  List.filter (fun number -> List.mem number winning) ours
  |> List.filter (fun el -> el <> "")
  |> List.map int_of_string

(* https://en.wikipedia.org/wiki/Exponentiation_by_squaring *)
let rec pow x n =
  if n < 0 then pow (1 / x) (-n)
  else if n = 0 then 1
  else if n mod 2 = 0 then pow (x * x) (n / 2)
  else x * pow (x * x) ((n - 1) / 2)

let calculate_score winning_numbers = pow 2 (List.length winning_numbers - 1)

let () =
  let input_channel = open_in "input.txt" in
  let lines = read_lines input_channel in
  let res =
    List.map
      (fun line ->
        String.split_on_char '|' (List.nth (String.split_on_char ':' line) 1))
      lines
    |> List.map winning_numbers |> List.map calculate_score
    |> List.fold_left ( + ) 0
  in
  print_int res

(* Answer: 26426 *)
