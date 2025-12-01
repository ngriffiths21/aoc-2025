open Core
open Lexing

exception SyntaxError of string

let print_position outx lexbuf =
  let pos = lexbuf.lex_curr_p in
  fprintf outx "file %s, row %d, col %d" pos.pos_fname pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error runparser lexer lexbuffer =
  try runparser lexer lexbuffer with
  | SyntaxError msg ->
    fprintf stderr "%a: %s\n" print_position lexbuffer msg;
    None
  | Day1_parser.Error ->
    fprintf stderr "%a: parsing error\n" print_position lexbuffer;
    exit (-1)