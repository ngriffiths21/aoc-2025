{
open Base
open Day4_parser

exception SyntaxError of string
}

let space = "."
let roll = "@"
let newline = "\n" | "\r\n"
let end = newline* eof

rule read =
    parse
      | space   { SPACE }
      | roll    { ROLL }
      | newline { Lexing.new_line lexbuf; NEWLINE }
      | end     { EOF }
      | _       { raise (SyntaxError ("Lexing error, unexpected char: " ^ Lexing.lexeme lexbuf))}
