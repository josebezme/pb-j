%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACKET RBRACKET COMMA BAR
%token MAP ARRAY STRING
%token NULL
%token COMMENT
%token ASSIGN
%token PRINT
%token <string> INT_LITERAL
%token <string> STRING_LITERAL
%token <string> ARRAY_LITERAL
%token <string> ID
%token EOF

%left  ID LBRACKET RBRACKET
%left ASSIGN

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

expr:
  ID ASSIGN expr   { Assign($1,$3) }
  | ID LBRACKET expr RBRACKET { GetArray($1, $3) }
  | STRING_LITERAL { StringLiteral($1) }
	| NULL           { Null } 
	| INT_LITERAL    { IntLiteral($1) }
	| LBRACKET array_list RBRACKET  { ArrayLiteral($2) }
	| BAR ID BAR     { ArrayLength($2) }
  | ID             { Id($1) }

vdecl:
  MAP ID { Map($2) }
  | ARRAY ID { Array($2) } 
  | STRING ID { String($2) }

adecl:
 ID { Id($1) } 

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

array_list:
    /* nothing */  { [] }
  | array_list li  { $2 :: $1 }

li:
    expr COMMA { Clear($1) }
	| expr       { Clear($1) }

stmt:
  vdecl SEMI                { Declare($1) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | vdecl ASSIGN expr SEMI  { DeclareAssign($1, $3) }
  | adecl LBRACKET expr RBRACKET ASSIGN expr SEMI { PutArray($1, $3, $6) }
	| PRINT LPAREN expr RPAREN SEMI { Print($3) }
  | expr SEMI               { Expr($1) }



