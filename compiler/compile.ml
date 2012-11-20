open Ast

type env = {
    function_index : string list; 
    global_index   : string list;
  }

let translate (globals, functions) =

  (* Translate a function with given env *)
  let translate env fdecl =
    let rec get_dt_name = function
      String(id) -> id
      | Map(id) -> id
      | Array(id) -> id
    in
    let rec default_init = function
      String(id) -> "\"\""
      | _ -> raise (Failure ("Not implemented yet."))
    in
    let rec data_types = function
      String(id) -> "String " ^ id
      | Map(id) -> "Map<Object, Object> " ^ id
      | Array(id) -> "List<Object> " ^ id
    in let rec string_expr locals = function
      StringLiteral(s) -> "\"" ^ s ^ "\""
      | Assign(s, e) -> s ^ " = " ^ string_expr locals e
      | Id(s) -> 
        if(List.exists (fun n -> n = s) locals) then
          s
        else
          raise (Failure ("Undeclared variable " ^ s))
    in
    let rec stmt (output, locals) = function
      Block(stmts) -> 
        let l = List.fold_left stmt ("", locals) stmts 
        in (output ^ "{\n" ^ (fst l) ^ "\n}\n", locals)
      | Print(s) -> (output ^ "System.out.println(" ^ string_expr locals s ^ ");\n", locals)
      | Expr(e) -> (output ^ string_expr locals e ^ ";\n", locals)
      | Declare(dt) -> 
        let name = get_dt_name dt in
        if List.exists (fun n -> n = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          (output ^ data_types dt ^ " = " ^ default_init dt ^ ";\n", name :: locals)
      | DeclareAssign(dt, e) -> 
        let name = get_dt_name dt in
        if List.exists (fun n -> n = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          (output ^ data_types dt ^ " = " ^ string_expr locals e ^ ";\n", name :: locals)

    in "\npublic static void " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map data_types fdecl.formals) ^ ")"
      ^ fst (stmt ("", []) (Block fdecl.body)) ^ "\n"
  in let env = { 
      function_index = [];
      global_index = []
    }
  in String.concat "\n" (List.map (translate env) functions)



