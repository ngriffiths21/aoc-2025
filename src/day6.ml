open Core

exception Applying_space

let rec compute_all operands ops total =
  match List.hd_exn operands with
    | [] -> total
    | _ ->
      let curr_nums = List.map operands ~f:List.hd_exn in
      let curr_op = List.hd_exn ops in
      let result = List.fold_left (List.drop curr_nums 1) ~init:(List.hd_exn curr_nums) ~f:curr_op in
      let remaining = List.map operands ~f:(Fn.flip List.drop 1) in
      compute_all remaining (List.drop ops 1) (total + result)

let find_next_start ops : int * [`Times | `Plus | `Space] list =
  let rec find_op i curr_ops =
    match curr_ops with
    | [] -> (i + 1, [])
    | `Space :: rest -> find_op (i + 1) rest
    | _ -> (i, curr_ops) in
  find_op 1 (List.drop ops 1)

let parse_down operands next_col_start : int list * int list list =
  let rec parse_operands pos opds parsed =
    (* skip the zeroes column *)
    if pos = next_col_start - 1 then
      (parsed, List.map opds ~f:(Fn.flip List.drop 1))
      (* drop the zeroes at the top of the operands stack *)
    else
      let new_digits = List.map opds ~f:List.hd_exn in
      let next_digit acc a = if a = 0 then acc else acc * 10 + a in
      let new_val = List.fold_left new_digits ~init:0 ~f:next_digit in
      let remaining = List.map opds ~f:(Fn.flip List.drop 1) in
      parse_operands (pos + 1) remaining (new_val :: parsed) in
  parse_operands 0 operands []


let rec compute_down operands ops total =
  match List.hd ops with
    | None -> total
    | Some op ->
      let next_col, next_ops = find_next_start ops in
      let curr_nums, next_opds = parse_down operands next_col in
      let op_fn = match op with
        | `Times -> ( * )
        | `Plus -> (+)
        | `Space -> raise Applying_space in
      let result = List.fold_left (List.drop curr_nums 1) ~init:(List.hd_exn curr_nums) ~f:op_fn in
      compute_down next_opds next_ops (total + result)


let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let input_p1 = Parse.parse_with_error Day6_parser.prog Day6_lexer.read lexbuf in
  printf "read %d lines and %d operators\n" (List.length (fst input_p1)) (List.length (snd input_p1));
  let file = In_channel.create filename in
  let lexbuf2 = Lexing.from_channel file in
  let input_p2 = Parse.parse_with_error Day6_part2_parser.prog Day6_part2_lexer.read lexbuf2 in
  let part1 = compute_all (fst input_p1) (snd input_p1) 0 in
  printf "part 1: %d\n" part1;
  let part2 = compute_down (fst input_p2) (snd input_p2) 0 in
  printf "part 2: %d\n" part2
