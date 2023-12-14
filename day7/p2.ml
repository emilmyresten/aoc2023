type hand =
  | FIVE_KIND of char list
  | FOUR_KIND of char list
  | FULL_HOUSE of char list
  | THREE_KIND of char list
  | TWO_PAIR of char list
  | ONE_PAIR of char list
  | HIGH_CARD of char list

let read_lines in_channel =
  let rec read_lines_aux lines in_channel =
    try
      let line = input_line in_channel in
      read_lines_aux (String.trim line :: lines) in_channel
    with _ -> lines
  in
  List.rev (read_lines_aux [] in_channel)

let print_lines lines =
  Printf.printf "%s"
    (String.concat "\n"
       (List.map (fun (hand, bid) -> hand ^ " " ^ string_of_int bid) lines))

let split_and_trim line =
  line |> String.split_on_char ' '
  |> List.filter (fun s -> String.empty <> s)
  |> List.map String.trim

let organize_line line = (List.hd line, int_of_string (List.hd (List.tl line)))

let _remove_duplicates hand =
  List.fold_left
    (fun acc c -> if not (List.mem c acc) then c :: acc else acc)
    [] hand

let group_label_by_amount hand =
  List.fold_left
    (fun acc m ->
      if List.mem_assoc m acc then
        let times_seen = List.assoc m acc + 1 in
        List.remove_assoc m acc |> List.cons (m, times_seen)
      else (m, 1) :: acc)
    [] hand

let print_hand_classification hand =
  Printf.printf "%s\n"
    (match hand with
    | FIVE_KIND _ -> "five kind"
    | FOUR_KIND _ -> "four kind"
    | THREE_KIND _ -> "three kind"
    | FULL_HOUSE _ -> "full house"
    | TWO_PAIR _ -> "two pair"
    | ONE_PAIR _ -> "one pair"
    | HIGH_CARD _ -> "high card")

let merge_jokers label_by_amount =
  let number_of_jokers =
    match List.assoc_opt 'J' label_by_amount with None -> 0 | Some v -> v
  in
  if number_of_jokers = 0 || number_of_jokers = 5 then label_by_amount
  else
    let with_jokers_removed = List.remove_assoc 'J' label_by_amount in
    let gets_jokers =
      with_jokers_removed
      |> List.sort (fun (_, a) (_, b) -> a - b)
      |> List.rev |> List.hd
    in
    List.remove_assoc (fst gets_jokers) with_jokers_removed
    |> List.cons (fst gets_jokers, snd gets_jokers + number_of_jokers)

let classify_hand hand =
  let hand_list = String.to_seq hand |> List.of_seq in
  let label_by_amount = group_label_by_amount hand_list |> merge_jokers in
  List.iter
    (fun (label, amount) -> Printf.printf "%c, %d\n" label amount)
    label_by_amount;
  let classification =
    match List.length label_by_amount with
    | 1 -> FIVE_KIND hand_list
    | 2 -> (
        let card_numbers =
          List.map snd label_by_amount |> List.sort (fun a b -> a - b)
        in
        match card_numbers with
        | 1 :: 4 :: _ -> FOUR_KIND hand_list
        | _ -> FULL_HOUSE hand_list)
    | 3 -> (
        let card_numbers =
          List.map snd label_by_amount |> List.sort (fun a b -> a - b)
        in
        (* List.iter (fun m -> print_int m) card_numbers; *)
        match card_numbers with
        | 1 :: 1 :: 3 :: _ -> THREE_KIND hand_list
        | _ -> TWO_PAIR hand_list)
    | 4 -> ONE_PAIR hand_list
    | 5 -> HIGH_CARD hand_list
    | _ -> failwith "to many items in hand."
  in
  print_hand_classification classification;
  classification

let get_card_value card =
  List.assoc card
    [
      ('2', 2);
      ('3', 3);
      ('4', 4);
      ('5', 5);
      ('6', 6);
      ('7', 7);
      ('8', 8);
      ('9', 9);
      ('T', 10);
      ('J', 1);
      ('Q', 12);
      ('K', 13);
      ('A', 14);
    ]

let rec compare_cards_by_label hand1 hand2 =
  match (hand1, hand2) with
  | h1 :: t1, h2 :: t2 -> (
      if h1 = h2 then compare_cards_by_label t1 t2
      else
        match get_card_value h1 > get_card_value h2 with
        | true -> `HAND_1
        | false -> `HAND_2)
  | _ -> failwith "shouldnt occur, same hand"

let sort_hands hands =
  List.map (fun (hand, bid) -> (classify_hand hand, bid)) hands
  |> List.sort (fun (hand1, _) (hand2, _) ->
         match (hand1, hand2) with
         | FIVE_KIND hand1, FIVE_KIND hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1)
         | FIVE_KIND _, _ -> 1
         | _, FIVE_KIND _ -> -1
         | FOUR_KIND hand1, FOUR_KIND hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1)
         | FOUR_KIND _, _ -> 1
         | _, FOUR_KIND _ -> -1
         | FULL_HOUSE hand1, FULL_HOUSE hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1)
         | FULL_HOUSE _, _ -> 1
         | _, FULL_HOUSE _ -> -1
         | THREE_KIND hand1, THREE_KIND hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1)
         | THREE_KIND _, _ -> 1
         | _, THREE_KIND _ -> -1
         | TWO_PAIR hand1, TWO_PAIR hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1)
         | TWO_PAIR _, _ -> 1
         | _, TWO_PAIR _ -> -1
         | ONE_PAIR hand1, ONE_PAIR hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1)
         | ONE_PAIR _, _ -> 1
         | _, ONE_PAIR _ -> -1
         | HIGH_CARD hand1, HIGH_CARD hand2 -> (
             match compare_cards_by_label hand1 hand2 with
             | `HAND_1 -> 1
             | `HAND_2 -> -1))

let () =
  let hands =
    read_lines (open_in "input.txt")
    |> List.map split_and_trim |> List.map organize_line
  in
  let total_winnings =
    sort_hands hands
    |> List.mapi (fun i (_, bid) ->
           Printf.printf "%d\n" bid;
           (i, bid))
    |> List.fold_left (fun acc (i, bid) -> acc + (bid * (i + 1))) 0
  in
  print_int total_winnings

(* 248750248 *)
