%{ open Ast 

let parse_error s = (* Called by the parser function on error *)
  print_endline s;
  flush stdout
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE COMMA
%token MAP ARRAY STRING LONG DOUBLE
%token COMMENT
%token ASSIGN
%token PRINT
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

expr:
  ID ASSIGN expr { Assign($1,$3) }
  | LONG_LITERAL { LongLiteral($1) }
  | DUB_LITERAL {DubLiteral($1)}
  | STRING_LITERAL { StringLiteral($1) }
  | ID { Id($1) }

vdecl:
  MAP ID { Map($2) }
  | LONG ID {Long ($2) }
  | DOUBLE ID {Double ($2)}
  | ARRAY ID { Array($2) } 
  | STRING ID { String($2) }

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
  vdecl SEMI { Declare($1) }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | vdecl ASSIGN expr SEMI { DeclareAssign($1, $3) }
  | PRINT LPAREN expr RPAREN SEMI { Print($3) }
  | expr SEMI { Expr($1) }



