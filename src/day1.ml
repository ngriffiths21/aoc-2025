open Core

let parse_input lexbuffer =
  match Parse.parse_with_error Day1_parser.prog Day1_lexer.read lexbuffer with
    | Some x -> x
    | None -> []

let () =
  let file = In_channel.create "inputs/day1-in.txt" in
  let lexbuffer = Lexing.from_channel file in
  let input = parse_input lexbuffer
  in List.iter ~f:(fun a -> printf "%d," a) input