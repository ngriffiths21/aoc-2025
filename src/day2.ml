open! Core

exception Bad_input of string

let input_to_strings parsed = List.map parsed ~f:(fun a -> (sprintf "%d" (fst a), sprintf "%d" (snd a)))

let get_input ?(validate=false) filename =
  let file = In_channel.create filename in
  let lexbuffer = Lexing.from_channel file in
  let parsed = Day2_parser.prog Day2_lexer.read lexbuffer in
  if validate then 
    let as_strings = input_to_strings parsed in
    if List.fold_left as_strings ~init:true ~f:(fun acc a -> acc && String.length (snd a) - String.length (fst a) <= 1)
    then parsed
    else raise (Bad_input "Digits aren't consistent")
  else parsed

let repeat num digits = num * (Int.pow 10 digits) + num

let%test_unit "repeat digits" =
  [%test_eq:int] (repeat 123 3) 123123

let rec repeats num digits times =
  if times = 1 then num
  else num + (Int.pow 10 digits) * repeats num digits (times - 1)

let%test_unit "repeats digits" =
  [%test_eq:int] (repeats 123 3 3) 123123123

let filter_range range acc num =
  if num >= (fst range) && num <= (snd range)
    then num :: acc
  else acc

let lower_str range = sprintf "%d" (fst range)
let upper_str range = sprintf "%d" (snd range)

let find_ids range : int list = 
  let lower = lower_str range in
  let upper = upper_str range in
  let num_chars = (String.length upper / 2) * 2 in
  let lower_trunc =
    if String.length lower < num_chars
      then sprintf "%d" (Int.pow 10 (num_chars - 1))
    else lower in
  let upper_trunc =
    if String.length upper > num_chars
      then sprintf "%d" (Int.pow 10 (num_chars) - 1)
    else upper in
  let repeat_lower = Int.of_string (String.prefix lower_trunc (num_chars / 2)) in
  let repeat_upper = Int.of_string (String.prefix upper_trunc (num_chars / 2)) in
  let prefixes = List.range repeat_lower (repeat_upper + 1) in
  let invalids = List.map prefixes ~f:(fun a -> repeat a (num_chars / 2)) in
  List.fold_left invalids ~init:[] ~f:(filter_range range)

let%test_unit "find ids with 11 and 22" =
  [%test_eq: int list] (find_ids (11, 22)) [22; 11]
let%test_unit "find ids with 9 and 22" =
  [%test_eq: int list] (find_ids (9, 22)) [22; 11]
let%test_unit "find ids with 77 and 101" =
  [%test_eq: int list] (find_ids (77, 101)) [99; 88; 77]

let rec get_next_ids range num num_repeats =
  let repeat_len = String.length (sprintf "%d" num) in
  let repeated = repeats num repeat_len num_repeats in
  if repeated > snd range then []
  else let (next_num, next_repeats) =
    if num + 1 = (Int.pow 10 repeat_len) then ((num + 1) / 10, num_repeats + 1)
    (* roll over to next repeat *)
    else (num + 1, num_repeats) in
  if repeated < fst range || num_repeats = 1 then get_next_ids range next_num next_repeats
  else repeated :: get_next_ids range next_num next_repeats

let ids_by_length range length =
  (* repeat is given as tuple (first digits, length) *)
  let lower = lower_str range in
  let first_digits_init = Int.of_string (String.prefix lower length) in
  let num_repeats_init = (String.length lower) / length in
  (* number of repeats to at most match length of lower string *)
  get_next_ids range first_digits_init num_repeats_init

let find_all_repeats range : int list =
  let repeat_lengths = List.range 1 (String.length (upper_str range) / 2 + 1) in
  (* for each possible repeat length, enumerate all repeats in range: *)
  let all_repeats = List.fold_left repeat_lengths ~init:[] ~f:(fun acc a -> List.append (ids_by_length range a) acc) in
  List.stable_dedup ~compare:(-) all_repeats

let%test_unit "find any size repeats with 11 and 22" =
  [%test_eq: int list] (find_all_repeats (11, 22)) [11; 22]
let%test_unit "find any size repeats with 9 and 22" =
  [%test_eq: int list] (find_all_repeats (9, 22)) [11; 22]
let%test_unit "find any size repeats with 111 and 112" =
  [%test_eq: int list] (find_all_repeats (111, 112)) [111]
let%test_unit "find any size repeats with 998 and 1111" =
  [%test_eq: int list] (find_all_repeats (998, 1111)) [1010; 1111; 999]

let run filename =
  let input = get_input filename in
  let invalid_ids = List.map input ~f:(fun a -> List.fold_left (find_ids a) ~init:0 ~f:(+)) in
  printf "part 1: %d\n" (List.fold_left invalid_ids ~init:0 ~f:(+));
  let invalid_ids_anysize = List.map input ~f:(fun a -> List.fold_left (find_all_repeats a) ~init:0 ~f:(+)) in
  printf "part 2: %d\n" (List.fold_left invalid_ids_anysize ~init:0 ~f:(+))
