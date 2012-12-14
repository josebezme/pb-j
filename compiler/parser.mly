%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout
%}

%token NULL
%token SEMI COLON LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET BAR
%token STAR PIPE CONCAT
%token MAP ARRAY STRING LONG DOUBLE BOOLEAN
%token PLUS MINUS TIMES DIVIDE MOD
%token SEQUAL PEQUAL GT GTE LT LTE AND OR
%token COMMENT
%token ASSIGN
%token RETURN
%token PRINT
%token <string> ARRAY_BEGIN
%token <bool> BOOLEAN_LITERAL
%token <string> STRING_LITERAL
%token <string> ARRAY_LITERAL
%token <string> ID
%token <string> LONG_LITERAL
%token <string> DUB_LITERAL
%token EOF

%left ID LBRACKET RBRACKET CONCAT
%right RETURN
%right ASSIGN
%left AND OR
%left SEQUAL PEQUAL
%left GT GTE LT LTE
%left PLUS MINUS
%left TIMES DIVIDE MOD

%start program
%type <Ast.program> program

%%

program:
  { [], [] }
  | program fdecl { fst $1, ($2 :: snd $1)}

fdecl:
	ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
	{{
		fname = $1;
		formals = $3;
		body = List.rev $6
		}}

formals_opt:
  { [] }
  | formal_list   { List.rev $1 }

formal_list:
  vdecl { [$1] }
  | formal_list COMMA vdecl { $3 :: $1 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }  

map_entry_list:
  { [] }
  | map_entry { [$1] }
  | map_entry_list COMMA map_entry { $3 :: $1 }

map_entry:
  | prim_literal COLON expr { ($1, $3) }

array_list:
    /* nothing */  { [] }
  | expr { [$1] }
  | array_list COMMA expr  { $3 :: $1 }

prim_literal:
  STRING_LITERAL { StringLiteral($1) }
  | LONG_LITERAL { LongLiteral($1) }
  | DUB_LITERAL {DubLiteral($1)}
  | BOOLEAN_LITERAL { BooleanLiteral($1) }

stmt_expr:
  ARRAY_BEGIN expr RBRACKET ASSIGN expr { ArrayPut($1, $2, $5) }
  | ID LBRACE expr RBRACE ASSIGN expr { MapPut($1, $3, $6) }
  | ID ASSIGN expr   { Assign($1,$3) }
  | ID LPAREN actuals_opt RPAREN { FunctionCall($1,$3) }    

expr:
  stmt_expr { StmtExpr($1) }
  | ID { Id($1) }
  | expr PLUS expr { Binop($1, Add, $3) }
  | expr MINUS expr { Binop($1, Sub, $3) }
  | expr TIMES expr { Binop($1, Mult, $3) }
  | expr DIVIDE expr { Binop($1, Div, $3) }
  | expr MOD expr { Binop($1, Mod, $3) }
  | expr SEQUAL expr { Binop($1, Seq, $3) }
  | expr PEQUAL expr { Binop($1, Peq, $3) }
  | expr GT expr { Binop($1, Greater, $3) }
  | expr GTE expr { Binop($1, Geq, $3) }
  | expr LT expr { Binop($1, Less, $3) }
  | expr LTE expr { Binop($1, Leq, $3) }
  | expr AND expr { Binop($1, And, $3) }
  | expr OR expr { Binop($1, Or, $3) }
  | NULL           { Null }
  | prim_literal { Literal($1) }
  | ARRAY_BEGIN expr RBRACKET { ArrayGet($1, $2) }
	| LBRACKET array_list RBRACKET  { ArrayLiteral(List.rev $2) }
  | LBRACE map_entry_list RBRACE { MapLiteral($2) }
  | ID LBRACE expr RBRACE { MapGet($1, $3) }
  | ID STAR { MapKeys($1) }
  | ID LBRACE STAR RBRACE { MapValues($1) }
  | PIPE ID PIPE { Size($2) }
  | expr CONCAT expr { Concat($1, $3) }

vdecl:
  MAP ID { Map($2) }
  | LONG ID {Long ($2) }
  | DOUBLE ID {Double ($2)}
  | ARRAY ID { Array($2) } 
  | STRING ID { String($2) }
  | BOOLEAN ID { Boolean($2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  vdecl SEMI { Declare($1) }
  | stmt_expr SEMI { ExprAsStmt($1) }
  | RETURN expr SEMI { Return($2) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | vdecl ASSIGN expr SEMI { DeclareAssign($1, $3) }
  | PRINT LPAREN expr RPAREN SEMI { Print($3) }



