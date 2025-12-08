open Core

let rec compute_all operands ops total =
  match List.hd_exn operands with
    | [] -> total
    | _ ->
      let curr_nums = List.map operands ~f:List.hd_exn in
      let curr_op = List.hd_exn ops in
      let result = List.fold_left (List.drop curr_nums 1) ~init:(List.hd_exn curr_nums) ~f:curr_op in
      let remaining = List.map operands ~f:(Fn.flip List.drop 1) in
      compute_all remaining (List.drop ops 1) (total + result)

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let input_p1 = Parse.parse_with_error Day6_parser.prog Day6_lexer.read lexbuf in
  printf "read %d lines and %d operators\n" (List.length (fst input_p1)) (List.length (snd input_p1));
  let file = In_channel.create filename in
  let lexbuf2 = Lexing.from_channel file in
  let input_p2 = Parse.parse_with_error Day6_part2_parser.prog Day6_part2_lexer.read lexbuf2 in
  print_s @@ [%sexp_of: int list list] @@ fst input_p1;
  print_s @@ [%sexp_of: int list list] @@ fst input_p2;
  let part1 = compute_all (fst input_p1) (snd input_p1) 0 in
  printf "part 1: %d\n" part1
