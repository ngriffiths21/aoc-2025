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

let filter_range range acc num =
  if num >= (fst range) && num <= (snd range)
    then num :: acc
  else acc

let find_ids range : int list = 
  let lower = sprintf "%d" (fst range) in
  let upper = sprintf "%d" (snd range) in
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

let run filename =
  let input = get_input filename in
  let invalid_ids = List.map input ~f:(fun a -> List.fold_left (find_ids a) ~init:0 ~f:(+)) in
  printf "part 1: %d\n" (List.fold_left invalid_ids ~init:0 ~f:(+))
