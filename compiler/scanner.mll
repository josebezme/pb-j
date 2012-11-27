{ open Parser }


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "..."        { comment lexbuf }
| '('          { LPAREN }
| ')'          { RPAREN }
| '{'          { LBRACE }
| '}'          { RBRACE }
| ';'          { SEMI }
| ':'          { COLON }
| ','          { COMMA }
| "map"        { MAP }
| "array"      { ARRAY }
| "print"      { PRINT }
| "string"     { STRING }
| "<-"         { ASSIGN }
| "->"         { RETURN }
| '"' ([^'"']+ as s) '"'   { STRING_LITERAL(s) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof      { EOF }
| _  as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
  | eof { EOF }
  | _ { comment lexbuf }