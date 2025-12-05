%token ROLL
%token SPACE
%token NEWLINE
%token EOF

%start <(int list) list> prog
%%

prog:
  rows = separated_list(NEWLINE, row); EOF { rows } ;

row:
  row = nonempty_list(cell) { row };

cell:
  | ROLL { 1 }
  | SPACE { 0 }