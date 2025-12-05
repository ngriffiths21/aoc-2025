open Core
open Lexing

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "file %s, row %d, col %d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error runparser lexer lexbuffer =
  try runparser lexer lexbuffer with
  | Day4_lexer.SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuffer msg;
    exit (-1)
  | Day4_parser.Error ->
    fprintf stderr "%a: parsing error\n" print_position lexbuffer;
    exit (-1)