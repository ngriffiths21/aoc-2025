{
open Base
open Day8_parser

exception SyntaxError of string
}

let newline = "\n" | "\r\n"
let num = ['0'-'9']+
let end = newline* eof

rule read =
  parse
    | ","       { COMMA }
    | num       { INT (Int.of_string (Lexing.lexeme lexbuf)) }
    | newline   { Lexing.new_line lexbuf; NEWLINE }
    | end       { EOF}
