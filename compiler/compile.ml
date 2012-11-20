open Ast

let translate (globals, functions) =

  (* Translate a function with given env *)
  let translate env fdecl =
    let rec data_types = function
      String(id) -> "String " ^ id
      | Map(id) -> "Map<Object, Object> " ^ id
      | Array(id) -> "List<Object> " ^ id
    in let rec string_expr = function
      StringLiteral(s) -> "\"" ^ s ^ "\""
      | Assign(dt, e) -> data_types dt ^ " = " ^ string_expr e
      | Id(s) -> s
    in
    let rec stmt = function
      Block(stmts) -> "{\n" ^ String.concat "\n" (List.map stmt stmts) ^ "\n}"
      | Print(s) -> "System.out.println(" ^ string_expr s ^ ");"
      | Expr(e) -> string_expr e ^ ";"
    in "\npublic static void " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map data_types fdecl.formals) ^ ")"
      ^ stmt (Block fdecl.body) ^ "\n"
  in String.concat "\n" (List.map (translate []) functions)