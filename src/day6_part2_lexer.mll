{
open Base
open Day6_part2_parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let newline = "\n" | "\r\n"
let space = " "
let end = newline* eof

rule read =
  parse
    | digit    { DIGIT (Int.of_string (Lexing.lexeme lexbuf)) }
    | space     { SPACE }
    | "+"       { PLUS }
    | "*"       { TIMES }
    | newline   { Lexing.new_line lexbuf; NEWLINE }
    | end       { EOF }