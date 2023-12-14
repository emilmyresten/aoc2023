let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines = Printf.printf "%s" (String.concat "\n" lines)

let rec find x lst c =
  match lst with
  | [] -> failwith "Not Found"
  | h :: t -> if h = x then c else find x t (c + 1)

let get_seeds lines =
  let rec seeds_aux seeds lines =
    match lines with
    | "" :: rest -> (List.flatten seeds, rest)
    | seed_line :: t ->
        seeds_aux
          ((List.tl (String.split_on_char ':' seed_line)
           |> List.map (fun seed_line ->
                  String.trim seed_line |> String.split_on_char ' '
                  |> List.map int_of_string)
           |> List.flatten)
          :: seeds)
          t
    | _ -> failwith "seed not in a single line"
  in
  let seeds, lines = seeds_aux [] lines in
  let seeds_with_ranges =
    let partitioned_by_index =
      List.partition (fun seed -> find seed seeds 0 mod 2 = 0) seeds
    in
    List.combine (fst partitioned_by_index) (snd partitioned_by_index)
  in

  (seeds_with_ranges, lines)

let get_next_map lines =
  let rec next_map_aux map lines =
    match lines with
    | [] -> (map, [])
    | "" :: lines -> (map, lines)
    | h :: t -> next_map_aux (h :: map) t
  in
  let next_map, lines = next_map_aux [] lines in
  let cleaned_next_map =
    List.tl (List.rev next_map)
    |> List.map String.trim
    |> List.map (fun m -> String.split_on_char ' ' m |> List.map int_of_string)
  in
  (cleaned_next_map, lines)

let find_map_for_number (number, range) map =
  let mapping =
    List.filter
      (fun potential_mapping ->
        match potential_mapping with
        | [ _; src; map_range ] ->
            number >= src && number <= src + range && range <= map_range
        | _ -> false)
      map
  in
  match mapping with
  | [] -> (
      try
        let potential_mapping =
          List.find
            (fun potential_mapping ->
              match potential_mapping with
              | [ _; src; map_range ] ->
                  number >= src && number <= src + range && range > map_range
              | _ -> false)
            map
        in
        potential_mapping
      with _ -> [ (number, range) ])
  | [ dest; src; map_range ] :: _ ->
      let offset = number - src in
      [ (dest + offset, range) ]
  | _ -> failwith "couldnt find mapping"

let find_mappings map domain =
  List.map (fun number -> find_map_for_number number map) domain |> List.flatten

let rec solve values maps =
  try
    let map, remaining_maps = get_next_map maps in
    let next_values = find_mappings map values in
    solve next_values remaining_maps
  with _end_of_file_exn -> values

let () =
  let input_channel = open_in "input.txt" in
  let lines = read_lines input_channel in
  let seeds, remaining_lines = get_seeds lines in
  let locations = solve seeds remaining_lines in
  let minimum_location =
    List.fold_left
      (fun acc location -> min acc (fst location))
      (fst (List.hd locations))
      (List.tl locations)
  in
  print_int minimum_location
