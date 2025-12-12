%token <int> INT
%token COMMA
%token NEWLINE
%token EOF

%start<(int * int * int) list> prog
%%

prog:
  vals = valrows; EOF { vals }

valrows:
  vr = separated_nonempty_list(NEWLINE, valrow) { vr }

valrow:
  i1 = INT; COMMA; i2 = INT; COMMA; i3 = INT { i1, i2, i3 }
