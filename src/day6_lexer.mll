{
open Base
open Day6_parser

exception SyntaxError of string
}

let number = ['0'-'9']+
let newline = "\n" | "\r\n"
let space = " "+
let end = space* newline* eof

rule read =
  parse
    | number    { INT (Int.of_string (Lexing.lexeme lexbuf)) }
    | space     { SPACE }
    | "+"       { PLUS }
    | "*"       { TIMES }
    | newline   { Lexing.new_line lexbuf; NEWLINE }
    | end       { EOF}