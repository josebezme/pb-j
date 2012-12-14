{ open Parser }


rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| "..."        { comment lexbuf }
| '('          { LPAREN }
| ')'          { RPAREN }
| '['          { LBRACKET }
| ']'          { RBRACKET }
| '{'          { LBRACE }
| '}'          { RBRACE }
| ';'          { SEMI }
| ':'          { COLON }
| ','          { COMMA }
| '|'          { PIPE }
| "**"         { STARSTAR }
| '~'          { CONCAT }
| '+'          { PLUS }
| '-'          { MINUS }
| '*'          { TIMES }
| '/'          { DIVIDE }
| '%'          { MOD }
| '='          { SEQUAL }
| "==="        { PEQUAL }
| '>'          { GT }
| ">="         { GTE }
| '<'          { LT }
| "<="         { LTE }
| "&&"         { AND }
| "||"         { OR }
| "map"        { MAP }
| "array"      { ARRAY }
| "print"      { PRINT }
| "string"     { STRING }
| "<-"         { ASSIGN }
| "boolean"    { BOOLEAN }
| "true"       { BOOLEAN_LITERAL(true) }
| "false"      { BOOLEAN_LITERAL(false) }
| "long"       { LONG }
| "double"     { DOUBLE }
| "null"       { NULL }
| (['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as id) "[" { ARRAY_BEGIN(id) }
| '"' ([^'"']+ as s) '"'   { STRING_LITERAL(s) }
| ['0'-'9']* ['.'] ['0'-'9']+ as lxm { DUB_LITERAL(lxm) }
| ['0'-'9']+ as lxm { LONG_LITERAL(lxm) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof          { EOF }
| "->"         { RETURN }
| _  as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
  | eof { EOF }
  | _ { comment lexbuf }