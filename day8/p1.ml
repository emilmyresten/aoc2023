let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines = Printf.printf "%s" (String.concat "\n" lines)

let rec parse_network lines =
  match lines with
  | h :: t ->
      let node = String.sub h 0 3
      and left = String.sub h 7 3
      and right = String.sub h 12 3 in
      (node, (left, right)) :: parse_network t
  | _ -> []

let rec goto_zzz ?(steps = 0) network pattern (left, right) =
  let next_choice = List.nth pattern (steps mod List.length pattern) in
  let steps = steps + 1 in
  match next_choice with
  | 'L' ->
      if left = "ZZZ" then steps
      else goto_zzz ~steps network pattern (List.assoc left network)
  | 'R' ->
      if right = "ZZZ" then steps
      else goto_zzz ~steps network pattern (List.assoc right network)
  | _ -> failwith "couldn't find way to zzz!"

let () =
  let lines = read_lines (open_in "input.txt") in
  let pattern = List.hd lines |> String.to_seq |> List.of_seq in
  let network = List.tl lines |> List.tl |> parse_network in
  Printf.printf "%d" (goto_zzz network pattern (List.assoc "AAA" network))

(* 19631 *)
