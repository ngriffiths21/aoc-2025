%token <int> DIGIT
%token SPACE
%token NEWLINE
%token PLUS
%token TIMES
%token EOF

%start<(int list list * [`Space | `Times | `Plus] list)> prog
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
  ops = nonempty_list(operator) { ops }

operator:
  | SPACE { `Space }
  | TIMES { `Times }
  | PLUS { `Plus }
