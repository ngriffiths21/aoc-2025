val print_position : out_channel -> Lexing.lexbuf -> unit

val parse_with_error :
  ('a -> Lexing.lexbuf -> 'b) -> 'a -> Lexing.lexbuf -> 'b