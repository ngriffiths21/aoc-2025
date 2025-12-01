{
open Base
open Day1_parser

exception SyntaxError of string
}

let number = ['0'-'9']+
let negative = "L"
let positive = "R"
let newline = "\n" | "\r\n"
let end = newline* eof

rule read =
    parse
      | number    { INT (Int.of_string (Lexing.lexeme lexbuf)) }
      | negative    { NEGATIVE }
      | positive    { read lexbuf }
      | newline     { Lexing.new_line lexbuf; SEP }
      | end         { EOF }
      | _           { raise (SyntaxError ("Lexing error, unexpected char: " ^ Lexing.lexeme lexbuf))}