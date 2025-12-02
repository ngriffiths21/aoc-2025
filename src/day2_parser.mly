%token <int> INT
%token EOF
%token DASH
%token COMMA

%start <(int * int) list> prog
%%

prog:
  | v = value; EOF { v } ;

value:
  v = separated_list(COMMA, range) { v };

range:
  lower = INT; DASH; upper = INT { (lower, upper) };