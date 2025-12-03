{
open Base
open Day3_parser

exception SyntaxError of string
}

let digit = ['0'-'9']
let newline = "\n" | "\r\n"
let end = newline* eof

rule read =
    parse
      | digit  { INT (Int.of_string (Lexing.lexeme lexbuf)) }
      | newline { NEWLINE }
      | end     { EOF }
      | _       { raise (SyntaxError ("Lexing error, unexpected char: " ^ Lexing.lexeme lexbuf))}
