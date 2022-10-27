%token LPAREN RPAREN
%token EOL
%token <int> INT
%token <float> FLOAT
%token TOINT TOFLOAT PLUS PLUSF MINUS MINUSF TIMES TIMESF DIV MOD
%left PLUS PLUSF MINUS MINUSF      
%left TIMES TIMESF DIV MOD      
%start parse
%type <Asyntax.exp> parse
%%
parse:  
  expr EOL  { $1 }
;
expr:
  INT  { Asyntax.Int($1)}
| FLOAT { Asyntax.Float($1)}
| TOFLOAT LPAREN expr RPAREN { Asyntax.FLO ($3) }
| TOINT LPAREN expr RPAREN { Asyntax.IN ($3) }
| LPAREN expr RPAREN { Asyntax.Par ($2) }
| expr PLUS expr { Asyntax.Plusi ($1,$3) }
| expr PLUSF expr { Asyntax.Plusf ($1,$3) }
| expr MINUS expr { Asyntax.Moinsi ($1,$3) }
| expr MINUSF expr { Asyntax.Moinsf ($1,$3) }
| expr TIMES expr { Asyntax.Multi ($1,$3) }
| expr TIMESF expr { Asyntax.Multf ($1,$3) }
| expr DIV expr { Asyntax.Divi ($1,$3) }
| expr MOD expr { Asyntax.Mod ($1,$3) }
| MINUS LPAREN expr RPAREN { Asyntax.Moins ($3) } 
| PLUS LPAREN expr RPAREN { $3 }
| MINUS INT { Asyntax.Moins (Asyntax.Int ($2))}
| PLUS INT { Asyntax.Plus (Asyntax.Int ($2))}
;