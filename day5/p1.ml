let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines = Printf.printf "%s" (String.concat "\n" lines)

let get_next_map lines =
  let rec next_map_aux map lines =
    match lines with
    | [] -> (map, [])
    | "" :: lines -> (map, lines)
    | h :: t -> next_map_aux (h :: map) t
  in
  let next_map, lines = next_map_aux [] lines in
  let cleaned_next_map =
    (if List.length next_map = 1 then
       List.tl (String.split_on_char ':' (List.hd next_map))
     else List.tl (List.rev next_map))
    |> List.map String.trim
    |> List.map (fun m -> String.split_on_char ' ' m |> List.map int_of_string)
  in

  (cleaned_next_map, lines)

let find_map_for_number number map =
  let mapping =
    List.filter
      (fun potential_mapping ->
        match potential_mapping with
        | [ _; src; range ] -> number >= src && number <= src + range
        | _ -> false)
      map
  in
  match mapping with
  | [] -> number
  | [ dest; src; range ] :: _ ->
      let offset = number - src in
      dest + offset
  | _ -> failwith "couldnt find mapping"

let find_mappings map domain =
  List.map (fun number -> find_map_for_number number map) domain

let rec solve values maps =
  try
    let map, remaining_maps = get_next_map maps in
    let next_values = find_mappings map values in
    solve next_values remaining_maps
  with _end_of_file_exn -> values

let () =
  let input_channel = open_in "input.txt" in
  let lines = read_lines input_channel in
  let seeds, remaining_lines = get_next_map lines in
  let locations = solve (List.hd seeds) remaining_lines in
  let minimum_location =
    List.fold_left min (List.hd locations) (List.tl locations)
  in
  print_int minimum_location
