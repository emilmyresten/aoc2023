[@@@warning "-partial-match"]

let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_line (direction, steps, color) =
  Printf.printf "%s %d %s\n" direction steps color

let print_lines lines = List.iter print_line lines

let parse_line line =
  let [ direction; steps; color ] = String.split_on_char ' ' line in
  (direction, int_of_string steps, color)

module Positions = struct
  include Set.Make (struct
    type t = int * int

    let compare = compare
  end)

  let pp ppf (row, col) = Format.fprintf ppf "(%d, %d)" row col
end

let direction_map =
  [ ("R", (0, 1)); ("L", (0, -1)); ("U", (-1, 0)); ("D", (1, 0)) ]

let dig_around lines =
  List.fold_left
    (fun ((current_row, current_col), visited) (direction, steps, _color) ->
      let direction = List.assoc direction direction_map in
      let steps =
        List.init steps succ
        |> List.map (fun step ->
               ( current_row + (fst direction * step),
                 current_col + (snd direction * step) ))
        |> List.to_seq
      in
      let row, col = steps |> List.of_seq |> List.rev |> List.hd in
      ((row, col), Positions.add_seq steps visited))
    ((0, 0), Positions.singleton (0, 0))
    lines
  |> snd

let rec flood_fill visited (current_row, current_col) =
  let directions = List.map snd direction_map in
  List.fold_left
    (fun visited (row, col) ->
      let pos = (current_row + row, current_col + col) in
      if Positions.mem pos visited then visited
      else flood_fill (Positions.add pos visited) pos)
    visited directions

let parse_lines lines = List.map parse_line lines

let print_positions positions =
  Positions.iter (fun pos -> Format.printf "%a " Positions.pp pos) positions

let () =
  let lines = read_lines (open_in "input.txt") |> parse_lines in
  let circumference = lines |> dig_around in
  let filled = flood_fill circumference (1, 1) in
  (* print_positions filled; *)
  Format.printf "%d\n" (Positions.cardinal filled)

(* 66993 *)
