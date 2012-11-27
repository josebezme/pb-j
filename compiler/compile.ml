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
    let into_map str = 
      "plt.pbj.util.MapUtil.toMap(" ^ str ^ ")"
    in
    let rec default_init = function
      String(id) -> "\"\""
      | Map(id) -> "new HashMap<Object, Object>()"
      | _ -> raise (Failure ("initialization not implemented yet."))
    in
    let rec data_types = function
      String(id) -> "String " ^ id
      | Map(id) -> "Map<Object, Object> " ^ id
      | Array(id) -> "List<Object> " ^ id
    in let rec string_literal = function
      StringLiteral(s) -> "\"" ^ s ^ "\""
    in let rec check_assign locals e = function
      String(es) -> (match e with
        (* if it's an id get the data type from locals list *)
        Id(o_s) -> let o_dt = List.find (fun dt -> get_dt_name dt = o_s) locals in
          ( (* Check the assignment of a variable *)
          match o_dt with
            String(s) -> true
            | _ -> raise (Failure ("Assigned string to invalid data type."))
          )
        | Literal(l) -> ( (* Check the assignment of a string to an id *)
          match l with 
            StringLiteral(sl) -> true
            | _ -> raise (Failure ("Assigned string to non string literal."))
          )
        | MapLiteral(ml) -> raise (Failure ("Assigned string to map literal."))
        | _ -> raise (Failure ("Assigned string to invalid value."))
        )
      | _ -> raise (Failure ("Not yet implemented check assignment for this dt."))
    in let rec string_expr locals = function
      | Literal(l) -> string_literal l
      | Assign(s, e) -> 
        let dt = List.find (fun dt -> get_dt_name dt = s) locals in
          if check_assign locals e dt then
            s ^ " = " ^ string_expr locals e
          else
            raise (Failure ("Failed check assign."))
      | MapLiteral(ml) -> into_map ("new Object[]{" ^
          String.concat "," (List.map (fun (d,e) -> string_literal d ^ "," ^ string_expr locals e) ml) ^ 
          "}")
      | MapGet(id, key) -> id ^ ".get(" ^ string_expr locals key ^ ")"
      | MapPut(id, key, v) -> id ^ ".put(" ^ string_expr locals key ^ ", " ^ string_expr locals v ^ ")"
      | Id(s) -> 
        if(List.exists (fun dt -> get_dt_name dt = s) locals) then
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
        if List.exists (fun dt -> get_dt_name dt = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          (output ^ data_types dt ^ " = " ^ default_init dt ^ ";\n", dt :: locals)
      | DeclareAssign(dt, e) -> 
        let name = get_dt_name dt in
        if List.exists (fun dt -> get_dt_name dt = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          if check_assign locals e dt then
            (output ^ data_types dt ^ " = " ^ string_expr locals e ^ ";\n", dt :: locals)
          else
            raise (Failure ("Failed check assign on declare assign."))

    in "\npublic static void " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map data_types fdecl.formals) ^ ")"
      ^ fst (stmt ("", []) (Block fdecl.body)) ^ "\n"
  in let env = { 
      function_index = [];
      global_index = []
    }
  in String.concat "\n" (List.map (translate env) functions)



