open Core

let run filename =
  let file = In_channel.create filename in
  let lexbuf = Lexing.from_channel file in
  let input = Parse.parse_with_error Day6_parser.prog Day6_lexer.read lexbuf in
  printf "read %d lines and %d operators\n" (List.length (fst input)) (List.length (snd input))