%token <int> INT
%token EOF
%token NEWLINE

%start <(int list) list> prog
%%

prog:
  bs = banks; EOF { bs } ;

banks:
  bs = separated_list(NEWLINE, bank) { bs };

bank:
  nums = nonempty_list(INT) { nums };