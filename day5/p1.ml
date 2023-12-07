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
  in
  (cleaned_next_map, lines)

let find_map_for_number number map =
  let number_int = int_of_string number in
  let mapping =
    List.filter
      (fun potential_mapping ->
        let potential_mapping =
          String.split_on_char ' ' potential_mapping |> List.map int_of_string
        in

        number_int >= List.nth potential_mapping 1
        && number_int
           <= List.nth potential_mapping 1 + List.nth potential_mapping 2)
      map
    |> List.map (String.split_on_char ' ')
  in
  match mapping with
  | [] -> number
  | m :: _ -> (
      match m with
      | dest :: src :: range ->
          let offset = number_int - int_of_string src in
          string_of_int (int_of_string dest + offset)
      | _ -> failwith "index error")

let find_mappings map domain =
  match domain with
  | domain :: [] ->
      let split_domain = String.split_on_char ' ' domain in
      [
        List.map (fun d -> find_map_for_number d map) split_domain
        |> String.concat " ";
      ]
  | _ ->
      print_lines domain;
      failwith "domain should be just one element"

let () =
  let input_channel = open_in "input.txt" in
  let lines = read_lines input_channel in
  let seeds, remaining_lines = get_next_map lines in
  let seed_to_soil, remaining_lines = get_next_map remaining_lines in
  let soil_to_fertilizer, remaining_lines = get_next_map remaining_lines in
  let fertilizer_to_water, remaining_lines = get_next_map remaining_lines in
  let water_to_light, remaining_lines = get_next_map remaining_lines in
  let light_to_temperature, remaining_lines = get_next_map remaining_lines in
  let temperature_to_humidity, remaining_lines = get_next_map remaining_lines in
  let humidity_to_location, remaining_lines = get_next_map remaining_lines in
  let locations =
    find_mappings seed_to_soil seeds
    |> find_mappings soil_to_fertilizer
    |> find_mappings fertilizer_to_water
    |> find_mappings water_to_light
    |> find_mappings light_to_temperature
    |> find_mappings temperature_to_humidity
    |> find_mappings humidity_to_location
    |> List.hd |> String.split_on_char ' ' |> List.map int_of_string
  in
  let minimum_location =
    List.fold_left min (List.hd locations) (List.tl locations)
  in
  print_int minimum_location
