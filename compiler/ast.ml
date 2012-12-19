type op = Add | Sub | Mult | Div | Mod | Seq | Peq | Greater | Geq | Less | Leq | Neq | And | Or

type data_type =
  String  of string
  | Map   of string
  | Array of string
  | Boolean of string
  | Long of string
  | Double of string
  | Void of string

type literal = 
  | StringLiteral of string
  | LongLiteral of string
  | DubLiteral of string
  | BooleanLiteral of bool

type stmt_expr = 
  Assign of string * expr
  | ArrayPut      of string * expr * expr
  | MapPut of string * expr * expr  
	| Spread    of stmt_expr
	| JamSpread of stmt_expr * stmt_expr
  | FunctionCall of string * expr list 
  | NoExpr 

and expr = 
  StmtExpr of stmt_expr
  | Id of string
  | Literal of literal
  | Binop of expr * op * expr
  | ArrayGet      of string * expr
  | ArrayLiteral  of expr list
  | Null
  | MapLiteral of (literal * expr) list
  | MapGet of string * expr
  | MapKeys of string
  | MapValues of string
  | Size of string
  | Concat of expr * expr
  | At of expr

type stmt =
    Block of stmt list
  | Print of expr
  | Return of expr
  | Declare of data_type
  | DeclareAssign of data_type * expr
  | If of expr * stmt * stmt
  | For of stmt * expr * stmt_expr * stmt
  | While of expr * stmt
  | DoWhile of stmt * expr
  | ExprAsStmt of stmt_expr
  | NoStmt

type func_decl = {
    fname : data_type;
    formals : data_type list;
    body : stmt list;
  }

type program = (data_type * literal) list * func_decl list

let rec string_of_data_type = function
  String(s) -> "STRING-" ^ s
  | Map(s) -> "MAP-" ^ s
  | Array(s) -> "ARRAY-" ^ s
  | Boolean(s) -> "BOOLEAN-" ^ s
  | Long(s) -> "LONG-" ^ s
  | Double(s) -> "DOUBLE-" ^ s 
  | Void(s) -> "VOID-" ^ s

let rec string_of_literal = function
  StringLiteral(s) -> "\"" ^ s ^ "\""
  | LongLiteral(l) -> "LONG_LIT: " ^ l
  | DubLiteral(l) -> "DUB_LIT: " ^ l
  | BooleanLiteral(s) -> string_of_bool s

let rec string_of_stmt_expr = function
  Assign(s, e) -> "ASSIGN " ^ s ^ " TO " ^ string_of_expr e
  | ArrayPut(id, idx, e)   -> "ARRAY-" ^ id ^ "-PUT[" ^ string_of_expr idx ^ ", " ^ string_of_expr e ^ "]"
  | MapPut(id,key,v) -> "MAP-" ^ id ^ "-PUT{" ^ string_of_expr key ^ ", " ^ string_of_expr v ^ "}"
  | FunctionCall(s,e) ->"FUNCTION CALL " ^ s ^   "{\n" ^ String.concat "," (List.map string_of_expr e) ^ "}\n"  
  | Spread(f)       -> "SPREAD AND CALL " ^ string_of_stmt_expr f
	| JamSpread(f, s) -> "JAM THE RESULTS OF " ^ string_of_stmt_expr s 
                           ^ " WITH " ^ string_of_stmt_expr f
  | NoExpr -> "NoStmtExpr"

and string_of_expr = function
  StmtExpr(e) -> string_of_stmt_expr e
  | Literal(l) -> string_of_literal l
  | Id(s) -> "ID:" ^ s
  | Binop(e1, o, e2) -> "BINOP:" ^ string_of_expr e1 ^ " " ^
      (match o with
	Add -> "PLUS"
      | Sub -> "MINUS"
      | Mult -> "TIMES"
      | Div -> "DIV"
      | Mod -> "MOD"
      | Seq -> "SEQUAL"
      | Peq -> "PEQUAL"
      | Greater -> "GT"
      | Geq -> "GTE"
      | Less -> "LT"
      | Leq -> "LTE"
      | And -> "AND"
      | Neq -> "NEQ"
      | Or -> "OR") ^ " " ^ string_of_expr e2
  | ArrayGet(id,idx) -> "ARRAY-" ^ id ^ "-GET[" ^ string_of_expr idx ^ "]"
  | ArrayLiteral(al) -> "ARRAY[" ^ String.concat "," 
        (List.map (fun e -> string_of_expr e ) al) ^ "]"
  | MapLiteral(ml) -> "MAP{" ^ String.concat "," 
        (List.map (fun (a,b) -> string_of_literal a ^ ":" ^ string_of_expr b) ml) ^ "}"
  | MapGet(id,key) -> "MAP-" ^ id ^ "-GET{" ^ string_of_expr key ^ "}"
  | MapKeys(id) -> "MAP-KEYS-" ^ id
  | MapValues(id) -> "MAP-VALUES-" ^ id
  | Size(id) -> "SIZE-of-" ^ id
  | Concat(e1, e2) -> "CONCAT(" ^ string_of_expr e1 ^ "," ^ string_of_expr e1 ^ ")"
  | Null -> "NULL"
	| At(e)            -> "SPREADING: " ^ string_of_expr e

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Print(str) -> "PRINT (" ^ string_of_expr str ^ ");\n"
  | Return(e) -> "RETURN " ^ string_of_expr e ^ ";\n"
	| If (p, t, f) -> 
        "IF " ^ string_of_expr p 
				    ^ " THEN DO "
            ^ string_of_stmt t
            ^ " ELSE DO " ^ string_of_stmt f ^ ";\n"
  | For (s, e, se, b) -> 
		   "DECLARE: " ^ string_of_stmt s
	          ^ " AND DO " ^ string_of_stmt b
						^ " WHILE " ^ string_of_expr e 
						^ " PERFORMING " ^ string_of_stmt_expr se ^ ";\n"
  | While (e, b) -> " WHILE " ^ string_of_expr e ^ " DO " ^ string_of_stmt b ^ ";\n"
  | DoWhile (b, e) -> " DO " ^ string_of_stmt b ^ " WHILE " ^ string_of_expr e ^ ";\n"
  | ExprAsStmt(e) -> "EXPR: " ^ string_of_stmt_expr e ^ ";\n"
  | Declare(dt) -> "DECLARE: " ^ string_of_data_type dt ^ ";\n"
  | DeclareAssign(dt, e) -> "DECLARE: " ^ string_of_data_type dt ^ 
      " AND ASSIGN: " ^ string_of_expr e ^ ";\n"
  | NoStmt -> "NoStmt"

let string_of_fdecl fdecl =
  "FUNCTION " ^ string_of_data_type fdecl.fname ^ "(" ^ 
    String.concat "," (List.map string_of_data_type fdecl.formals) ^ 
    ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_vdecl id = "long " ^ id ^ ";\n"

let string_of_globals globals = 
  "GLOBALS " ^ string_of_data_type (fst globals) ^ " = " ^ string_of_literal (snd globals)   

let string_of_program (vars, funcs) =
  "Vars: \n" ^ String.concat ";\n" (List.map string_of_globals vars) ^ "\n" ^
  "Funcs: \n" ^ String.concat "\n" (List.map string_of_fdecl funcs)




