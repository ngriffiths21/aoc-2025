%token <int> DIGIT
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
  ns = nonempty_list(valcell); NEWLINE { ns }

valcell:
  | SPACE { 0 }
  | d = DIGIT { d }

operators:
  ops = separated_nonempty_list(nonempty_list(SPACE), operator) { ops }

operator:
  | TIMES { ( * ) }
  | PLUS { (+) }
