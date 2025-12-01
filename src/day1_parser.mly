%token <int> INT
%token EOF
%token NEGATIVE
%token SEP

%start <int list option> prog
%%

prog:
  | EOF               { None }
  | v = value; EOF    { Some v } ;

value:
  v = separated_list(SEP, number) { v };

number:
  | NEGATIVE; i = INT { (-i) }
  | i = INT           { i } ;