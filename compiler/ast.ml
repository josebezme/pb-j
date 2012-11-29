type data_type =
  String of string
  | Map of string
  | Array of string
  | Boolean of string
  | Long of string
  | Double of string

type literal = 
  | StringLiteral of string
  | LongLiteral of string
  | DubLiteral of string
  | BooleanLiteral of bool

type expr = 
  Assign of string * expr
  | Id of string
  | Literal of literal
  | MapLiteral of (literal * expr) list
  | MapGet of string * expr
  | MapPut of string * expr * expr

type stmt =
    Block of stmt list
  | Print of expr
  | Return of expr
  | Declare of data_type
  | DeclareAssign of data_type * expr
  | Expr of expr

type func_decl = {
    fname : string;
    formals : data_type list;
    body : stmt list;
  }

type program = data_type list * func_decl list

let rec string_of_data_type = function
  String(s) -> "STRING " ^ s
  | Map(s) -> "MAP " ^ s
  | Array(s) -> "ARRAY " ^ s
  | Boolean(s) -> "BOOLEAN" ^ s
  | Long(s) -> "LONG" ^ s
  | Double(s) -> "DOUBLE " ^ s 

let rec string_of_literal = function
  StringLiteral(s) -> "\"" ^ s ^ "\""
  | LongLiteral(l) -> "LONG_LIT: " ^ l
  | DubLiteral(l) -> "DUB_LIT: " ^ l
  | BooleanLiteral(s) -> string_of_bool s

let rec string_of_expr = function
  Assign(s, e) -> "ASSIGN " ^ s ^ " TO " ^ string_of_expr e
  | Literal(l) -> string_of_literal l
  | Id(s) -> "ID:" ^ s
  | MapLiteral(ml) -> "MAP{" ^ String.concat "," 
        (List.map (fun (a,b) -> string_of_literal a ^ ":" ^ string_of_expr b) ml) ^ "}"
  | MapGet(id,key) -> "MAP-" ^ id ^ "-GET{" ^ string_of_expr key ^ "}"
  | MapPut(id,key,v) -> "MAP-" ^ id ^ "-PUT{" ^ string_of_expr key ^ ", " ^ string_of_expr v ^ "}"

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Print(str) -> "PRINT (" ^ string_of_expr str ^ ");\n"
  | Return(e) -> "RETURN " ^ string_of_expr e ^ ";\n"
  | Expr(e) -> "EXPR: " ^ string_of_expr e ^ ";\n"
  | Declare(dt) -> "DECLARE: " ^ string_of_data_type dt ^ ";\n"
  | DeclareAssign(dt, e) -> "DECLARE: " ^ string_of_data_type dt ^ 
      " AND ASSIGN: " ^ string_of_expr e ^ ";\n"

let string_of_fdecl fdecl =
  "FUNCTION " ^ fdecl.fname ^ "(" ^ 
    String.concat "," (List.map string_of_data_type fdecl.formals) ^ 
    ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_vdecl id = "long " ^ id ^ ";\n"

let string_of_program (vars, funcs) =
  "Vars: \n" ^ String.concat ";\n" (List.map string_of_data_type vars) ^ "\n" ^
  "Funcs: \n" ^ String.concat "\n" (List.map string_of_fdecl funcs)


