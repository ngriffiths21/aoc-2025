%token <int> INT
%token DASH
%token NEWLINE
%token SECTIONEND
%token EOF

%start<((int * int) list) * int list> prog
%%

prog:
  rs = ranges; SECTIONEND; ids = ids; EOF { rs, ids }

ranges:
  rs = separated_list(NEWLINE, range) { rs }

range:
  lower = INT; DASH; upper = INT { lower, upper }

ids:
  ids = separated_list(NEWLINE, INT) { ids }
