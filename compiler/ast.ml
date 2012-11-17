type data_type =
  String of string
  | Map of string
  | Array of string

type string_expr =
  StringLiteral of string

type stmt =
    Block of stmt list
  | Print of string_expr

type func_decl = {
    fname : string;
    formals : data_type list;
    locals : data_type list;
    body : stmt list;
  }

type program = data_type list * func_decl list

let rec string_of_str_expr = function
  StringLiteral(s) -> s

let rec string_of_stmt = function
    Block(stmts) ->
      "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Print(str) -> "print(" ^ string_of_str_expr str ^ ");\n"

let rec string_of_vdecl = function
  String(s) -> "string " ^ s ^ ";\n"
  | Map(s) -> "map " ^ s ^ ";\n"
  | Array(s) -> "array " ^ s ^ ";\n"

let string_of_fdecl fdecl =
  fdecl.fname ^ "(" ^ String.concat "" (List.map string_of_vdecl fdecl.formals) ^ ")\n{\n" ^
  String.concat "" (List.map string_of_vdecl fdecl.locals) ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  String.concat "" (List.map string_of_vdecl vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)