{
open Base
open Day7_parser

exception SyntaxError of string
}

let newline = "\n" | "\r\n"
let end = newline* eof

rule read =
  parse
    | "S"    { START }
    | "."     { SPACE }
    | "^"       { SPLIT }
    | newline   { Lexing.new_line lexbuf; NEWLINE }
    | end       { EOF}
