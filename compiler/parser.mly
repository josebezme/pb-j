%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA BAR
%token MAP ARRAY STRING
%token NULL
%token SEMI COLON LPAREN RPAREN LBRACE RBRACE COMMA LBRACKET RBRACKET
%token STAR PIPE CONCAT
%token MAP ARRAY STRING LONG DOUBLE BOOLEAN
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

%start program
%type <Ast.program> program

%%
/*
%token AT SPREAD JAM

%right JAM
%right NOJAMFUN
%right SPREAD
*/
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
/*	| SPREAD stmt_expr             { Spread($2) }
	| JAM stmt_expr stmt_expr      { JamSpread($2, $3) }
	| JAM %prec NOJAMFUN stmt_expr { JamSpread( FunctionCall("stdJam", []), $2) }
*/
expr:
  stmt_expr { StmtExpr($1) }
  | ID { Id($1) }
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
/*	| AT expr        { At($2) }
*/
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



