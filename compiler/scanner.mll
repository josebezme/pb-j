{ open Parser }

rule token = parse
  [' ' '\t' '\r' '\n'] { token lexbuf }
| '('          { LPAREN }
| ')'          { RPAREN }
| '{'          { LBRACE }
| '}'          { RBRACE }
| ';'          { SEMI }
| ','          { COMMA }
| "map"        { MAP }
| "array"      { ARRAY }
| '"' _* '"'   { STRING }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof      { EOF }
| _  as char { raise (Failure("illegal character " ^ Char.escaped char)) }