type data_type =
  String of string
  | Map of string
  | Array of string

type expr = 
  Assign of string * expr
  | Id of string
  | StringLiteral of string

type stmt =
    Block of stmt list
  | Print of expr
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

let rec string_of_expr = function
  Assign(s, e) -> "ASSIGN " ^ s ^ " TO " ^ string_of_expr e ^ "\n"
  | StringLiteral(s) -> "\"" ^ s ^ "\""
  | Id(s) -> "ID:" ^ s

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Print(str) -> "PRINT (" ^ string_of_expr str ^ ");\n"
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

let string_of_program (vars, funcs) =
  "Vars: \n" ^ String.concat ";\n" (List.map string_of_data_type vars) ^ "\n" ^
  "Funcs: \n" ^ String.concat "\n" (List.map string_of_fdecl funcs)


