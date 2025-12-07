%token START
%token SPLIT
%token NEWLINE
%token SPACE
%token EOF

%start<([`Space | `Split | `Start ] list list)> prog
%%

prog:
  rows = rows; EOF { rows }

rows:
  rows = separated_nonempty_list(NEWLINE, row) { rows }

row:
  row = nonempty_list(cell) { row }

cell:
  | START { `Start }
  | SPLIT { `Split }
  | SPACE { `Space }