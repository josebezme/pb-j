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
| ';'          { SEMI   }
| ','          { COMMA  }
| '|'          { BAR    }
| "map"        { MAP    }
| "array"      { ARRAY  }
| "print"      { PRINT  }
| "string"     { STRING }
| "<-"         { ASSIGN }
| "null"       { NULL   }(*null is not a string*)
| '"' ([^'"']+ as s) '"'   { STRING_LITERAL(s) }
(*| '[' ([^']']+ as s) ']'   { ARRAY_LITERAL(s)  }*)
| ['0'-'9']+ as s { INT_LITERAL(s) }
| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
| eof      { EOF }
| _  as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and comment = parse
  '\n' { token lexbuf }
  | eof { EOF }
  | _ { comment lexbuf }