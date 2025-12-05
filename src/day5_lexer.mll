{
open Base
open Day5_parser

exception SyntaxError of string
}

let number = ['0'-'9']+
let newline = "\n" | "\r\n"
let end = newline* eof
let section_end = newline newline

rule read =
  parse
    | number    { INT (Int.of_string (Lexing.lexeme lexbuf)) }
    | "-"       { DASH }
    | newline   { Lexing.new_line lexbuf; NEWLINE }
    | section_end { SECTIONEND }
    | end       { EOF}