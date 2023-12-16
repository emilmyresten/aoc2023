type tile =
  | PIPE_VERTICAL of int * int
  | PIPE_HORIZONTAL of int * int
  | PIPE_NORTH_EAST of int * int
  | PIPE_NORTH_WEST of int * int
  | PIPE_SOUTH_WEST of int * int
  | PIPE_SOUTH_EAST of int * int
  | GROUND of int * int
  | STARTING_POSITION of int * int

let string_of_tile tile =
  match tile with
  | PIPE_VERTICAL (row, col) -> Printf.sprintf "| at %d %d" row col
  | PIPE_HORIZONTAL (row, col) -> Printf.sprintf "- at %d %d" row col
  | PIPE_NORTH_EAST (row, col) -> Printf.sprintf "L at %d %d" row col
  | PIPE_NORTH_WEST (row, col) -> Printf.sprintf "J at %d %d" row col
  | PIPE_SOUTH_WEST (row, col) -> Printf.sprintf "7 at %d %d" row col
  | PIPE_SOUTH_EAST (row, col) -> Printf.sprintf "F at %d %d" row col
  | GROUND (row, col) -> Printf.sprintf ". at %d %d" row col
  | STARTING_POSITION (row, col) -> Printf.sprintf "S at %d %d" row col

let directions = [ `NORTH; `SOUTH; `EAST; `WEST ]

let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_tiles tiles =
  Printf.printf "%s\n" (String.concat "\n" (List.map string_of_tile tiles))

let print_directions directions =
  Printf.printf "[%s]\n"
    (String.concat " "
       (List.map
          (fun dir ->
            match dir with
            | `NORTH -> "North"
            | `SOUTH -> "South"
            | `EAST -> "East"
            | `WEST -> "West")
          directions))

let parse_to_tiles lines =
  List.map (fun line -> String.to_seq line |> List.of_seq) lines
  |> List.mapi (fun row_num row ->
         List.mapi
           (fun col_num symbol ->
             match symbol with
             | '|' -> PIPE_VERTICAL (row_num, col_num)
             | '-' -> PIPE_HORIZONTAL (row_num, col_num)
             | 'L' -> PIPE_NORTH_EAST (row_num, col_num)
             | 'J' -> PIPE_NORTH_WEST (row_num, col_num)
             | '7' -> PIPE_SOUTH_WEST (row_num, col_num)
             | 'F' -> PIPE_SOUTH_EAST (row_num, col_num)
             | '.' -> GROUND (row_num, col_num)
             | 'S' -> STARTING_POSITION (row_num, col_num)
             | _ -> failwith "Unmapped tile")
           row)

let get_start_position tiles =
  List.filter
    (fun row ->
      List.exists
        (fun tile -> match tile with STARTING_POSITION _ -> true | _ -> false)
        row)
    tiles
  |> List.hd
  |> List.find (fun tile ->
         match tile with STARTING_POSITION _ -> true | _ -> false)

let get_tile rowi coli tiles =
  try
    let row_opt = List.nth_opt tiles rowi in
    match row_opt with Some row -> List.nth_opt row coli | None -> None
  with _ -> None

let get_neighbour_indexes rowi coli =
  [ (rowi - 1, coli); (rowi, coli - 1); (rowi, coli + 1); (rowi + 1, coli) ]

let possible_directions_from_tile tile =
  match tile with
  | PIPE_VERTICAL _ -> [ `NORTH; `SOUTH ]
  | PIPE_HORIZONTAL _ -> [ `EAST; `WEST ]
  | PIPE_NORTH_EAST _ -> [ `NORTH; `EAST ]
  | PIPE_NORTH_WEST _ -> [ `NORTH; `WEST ]
  | PIPE_SOUTH_WEST _ -> [ `SOUTH; `WEST ]
  | PIPE_SOUTH_EAST _ -> [ `SOUTH; `EAST ]
  | GROUND _ -> []
  | STARTING_POSITION _ -> [ `NORTH; `SOUTH; `EAST; `WEST ]

let get_position tile =
  match tile with
  | PIPE_VERTICAL (row, col) -> (row, col)
  | PIPE_HORIZONTAL (row, col) -> (row, col)
  | PIPE_NORTH_EAST (row, col) -> (row, col)
  | PIPE_NORTH_WEST (row, col) -> (row, col)
  | PIPE_SOUTH_WEST (row, col) -> (row, col)
  | PIPE_SOUTH_EAST (row, col) -> (row, col)
  | GROUND (row, col) -> (row, col)
  | STARTING_POSITION (row, col) -> (row, col)

let get_tile_in_direction direction tile tiles =
  let rowi, coli = get_position tile in
  match direction with
  | `NORTH -> get_tile (rowi - 1) coli tiles
  | `SOUTH -> get_tile (rowi + 1) coli tiles
  | `EAST -> get_tile rowi (coli + 1) tiles
  | `WEST -> get_tile rowi (coli - 1) tiles

let is_next_choice relative_direction tile =
  match relative_direction with
  | `NORTH -> (
      match tile with
      | PIPE_VERTICAL _ | PIPE_SOUTH_WEST _ | PIPE_SOUTH_EAST _
      | STARTING_POSITION _ ->
          true
      | _ -> false)
  | `SOUTH -> (
      match tile with
      | PIPE_VERTICAL _ | PIPE_NORTH_WEST _ | PIPE_NORTH_EAST _
      | STARTING_POSITION _ ->
          true
      | _ -> false)
  | `EAST -> (
      match tile with
      | PIPE_HORIZONTAL _ | PIPE_SOUTH_WEST _ | PIPE_NORTH_WEST _
      | STARTING_POSITION _ ->
          true
      | _ -> false)
  | `WEST -> (
      match tile with
      | PIPE_HORIZONTAL _ | PIPE_SOUTH_EAST _ | PIPE_NORTH_EAST _
      | STARTING_POSITION _ ->
          true
      | _ -> false)

let get_next_tile possible_directions tile tiles =
  List.map
    (fun direction -> get_tile_in_direction direction tile tiles)
    possible_directions
  |> List.filter (fun tile_opt ->
         match tile_opt with Some _ -> true | None -> false)
  |> List.map (fun tile_opt ->
         match tile_opt with
         | Some tile -> tile
         | None -> failwith "Nones should have been filtered")
  |> List.combine possible_directions
  |> List.filter (fun (direction, tile) -> is_next_choice direction tile)
  |> List.hd

let invert_direction direction =
  match direction with
  | `NORTH -> `SOUTH
  | `SOUTH -> `NORTH
  | `EAST -> `WEST
  | `WEST -> `EAST

let rec get_cycle ?(possible_directions = directions) ?(path_acc = []) tile
    tiles =
  (* print_tiles [ tile ]; *)
  match tile with
  | STARTING_POSITION _ when not (List.is_empty path_acc) -> path_acc
  | _ ->
      let direction_taken, next_tile =
        get_next_tile possible_directions tile tiles
      in
      let possible_directions =
        List.filter
          (fun direction -> invert_direction direction_taken <> direction)
          (possible_directions_from_tile next_tile)
      in
      get_cycle ~possible_directions ~path_acc:(next_tile :: path_acc) next_tile
        tiles

let () =
  let tiles = read_lines (open_in "input.txt") |> parse_to_tiles in
  let start_tile = get_start_position tiles in
  let path = get_cycle start_tile tiles in
  Printf.printf "%d\n" (List.length path / 2)

(* 6882 *)
