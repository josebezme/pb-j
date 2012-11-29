%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout
%}

%token SEMI COLON LPAREN RPAREN LBRACE RBRACE COMMA
%token STAR PIPE
%token MAP ARRAY STRING LONG DOUBLE BOOLEAN
%token COMMENT
%token ASSIGN
%token RETURN
%token PRINT
%token <bool> BOOLEAN_LITERAL
%token <string> STRING_LITERAL
%token <string> ID
%token <string> LONG_LITERAL
%token <string> DUB_LITERAL
%token EOF

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

map_entry_list:
  { [] }
  | map_entry { [$1] }
  | map_entry_list COMMA map_entry { $3 :: $1 }

map_entry:
  | prim_literal COLON expr { ($1, $3) }

prim_literal:
  STRING_LITERAL { StringLiteral($1) }
  | LONG_LITERAL { LongLiteral($1) }
  | DUB_LITERAL {DubLiteral($1)}
  | BOOLEAN_LITERAL { BooleanLiteral($1) }  

expr:
  ID ASSIGN expr { Assign($1,$3) }
  | LBRACE map_entry_list RBRACE { MapLiteral($2) }
  | prim_literal { Literal($1) }
  | ID { Id($1) }
  | ID LBRACE expr RBRACE ASSIGN expr { MapPut($1, $3, $6) }
  | ID LBRACE expr RBRACE { MapGet($1, $3) }
  | ID STAR { MapKeys($1) }
  | ID LBRACE STAR RBRACE { MapValues($1) }

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
  | expr SEMI { Expr($1) }
  | expr RETURN SEMI { Return($1) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | vdecl ASSIGN expr SEMI { DeclareAssign($1, $3) }
  | PRINT LPAREN expr RPAREN SEMI { Print($3) }
  



