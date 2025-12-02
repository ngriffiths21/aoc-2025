{
open Base
open Day2_parser

exception SyntaxError of string
}

let number = ['0'-'9']+
let dash = "-"
let comma = ","
let newline = "\n" | "\r\n"
let end = newline* eof

rule read =
    parse
      | number  { INT (Int.of_string (Lexing.lexeme lexbuf)) }
      | dash    { DASH }
      | comma   { COMMA }
      | end     { EOF }
      | _       { raise (SyntaxError ("Lexing error, unexpected char: " ^ Lexing.lexeme lexbuf))}