%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token MAP ARRAY
%token COMMENT
%token PRINT
%token <string> STRING
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%%

program:
  { [], [] }
  | program fdecl { fst $1, ($2 :: snd $1)}

fdecl:
	ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
	{{
		fname = $1;
		formals = $3;
		locals = List.rev $6;
		body = List.rev $7
		}}

formals_opt:
  { [] }
  | formal_list   { List.rev $1 }

formal_list:
  vdecl                     { [$1] }
  | formal_list COMMA vdecl { $3 :: $1 }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
  MAP ID { Map($2) }
  | ARRAY ID { Array($2) } 

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  PRINT LPAREN string_expr RPAREN SEMI { Print($3) }

string_expr:
  STRING { StringLiteral($1) }



