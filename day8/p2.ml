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

let rec _goto_zzz ?(steps = 0) network pattern (left, right) =
  let next_choice = List.nth pattern (steps mod List.length pattern) in
  let steps = steps + 1 in
  match next_choice with
  | 'L' ->
      if left = "ZZZ" then steps
      else _goto_zzz ~steps network pattern (List.assoc left network)
  | 'R' ->
      if right = "ZZZ" then steps
      else _goto_zzz ~steps network pattern (List.assoc right network)
  | _ -> failwith "couldn't find way to zzz!"

let step_network steps_taken network pattern point =
  let next_choice = List.nth pattern (steps_taken mod List.length pattern) in
  let left, right = List.assoc point network in
  (* Printf.printf "%s, %s\n" left right; *)
  match next_choice with
  | 'L' -> left
  | 'R' -> right
  | _ -> failwith "couldn't find way to zzz!"

let rec goto_zzz ?(steps = 0) network pattern nodes =
  let stepped =
    List.map (fun node -> step_network steps network pattern node) nodes
  in
  let steps = steps + 1 in
  let is_done = List.for_all (fun node -> String.ends_with ~suffix:"Z" node) in
  if is_done stepped then steps else goto_zzz ~steps network pattern stepped

let rec gcd_euclids a b =
  let r = a mod b in
  if r <> 0 then gcd_euclids b r else b

let lcm a b = a / gcd_euclids a b * b
let get_lcm lst = List.fold_left lcm 1 lst

let () =
  let lines = read_lines (open_in "input.txt") in
  let pattern = List.hd lines |> String.to_seq |> List.of_seq in
  let network = List.tl lines |> List.tl |> parse_network in
  let start_nodes =
    List.filter
      (fun (node, (_, _)) -> String.ends_with ~suffix:"A" node)
      network
  in
  let steps_for_each =
    List.map
      (fun node -> goto_zzz network pattern [ node ])
      (List.map fst start_nodes)
  in
  Printf.printf "%d\n" (get_lcm steps_for_each)

(* 21003205388413 *)
