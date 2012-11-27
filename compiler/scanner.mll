{ open Parser }


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "..."        { comment lexbuf }
| '('          { LPAREN }
| ')'          { RPAREN }
| '{'          { LBRACE }
| '}'          { RBRACE }
| ';'          { SEMI }
| ','          { COMMA }
| "map"        { MAP }
| "array"      { ARRAY }
| "print"      { PRINT }
| "string"     { STRING }
| "<-"         { ASSIGN }
| "long"	   { LONG }
| "double"	   { DOUBLE }
| '"' ([^'"']+ as s) '"'   { STRING_LITERAL(s) }
| ['0'-'9']+ as lxm { LONG_LITERAL(lxm) }
| ['0'-'9']* ['.'] ['0'-'9']+ as lxm { DUB_LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof      { EOF }
| _  as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
  | eof { EOF }
  | _ { comment lexbuf }