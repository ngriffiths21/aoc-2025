%token <int> INT
%token SPACE
%token NEWLINE
%token PLUS
%token TIMES
%token EOF

%start<(int list list * (int -> int -> int) list)> prog
%%

prog:
  vals = valrows; ops = operators; EOF { vals, ops }

valrows:
  vr = nonempty_list(valrow) { vr }

valrow:
  ns = separated_nonempty_list(SPACE, INT); NEWLINE { ns }

operators:
  ops = separated_nonempty_list(SPACE, operator) { ops }

operator:
  | TIMES { ( * ) }
  | PLUS { (+) }
