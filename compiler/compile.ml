open Ast

type env = {
    function_index : string list; 
    global_index   : string list;
  }

let translate (globals, functions) =

  (* Translate a function with given env *)
  let translate_helper env fdecl =
    (* This just returns the string name for a data type *)
    let rec get_dt_name = function
      String(id) -> id
      | Map(id) -> id
      | Array(id) -> id
    in
    (* This is a utility method for turning a map literal into a valid java expr
        The method it's self is in the backend code. *)
    let into_map str = 
      "plt.pbj.util.MapUtil.toMap(" ^ str ^ ")"
    in
    (* Returns the default java initialization for a data type. *)
    let rec default_init = function
      String(id) -> "\"\""
      | Map(id) -> "new HashMap<Object, Object>()"
      | _ -> raise (Failure ("initialization not implemented yet."))
    in
    (* Returns the java declaration of a datatype *)
    let rec string_of_data_type = function
      String(id) -> "String " ^ id
      | Map(id) -> "Map<Object, Object> " ^ id
      | Array(id) -> "List<Object> " ^ id
    (* Turns a literal object into a java expr *)
    in let rec string_of_literal = function
      StringLiteral(s) -> "\"" ^ s ^ "\""
    (* Checks for invalid assignments of data types *)
    in let rec check_assign locals e = function
      (*  CHECK ASSIGN FOR STRING ***********************************************)
      String(es) -> (match e with
        (* if it's an id get the data type from locals list *)
        Id(o_s) -> let o_dt = List.find (fun dt -> get_dt_name dt = o_s) locals in
          ( (* Check the assignment of a variable *)
          match o_dt with
            String(s) -> true
            | _ -> raise (Failure ("Assigned string to invalid data type."))
          )
        | Literal(l) -> 
          ( (* Check the assignment of a string to an id *)
          match l with 
            StringLiteral(sl) -> true
            | _ -> raise (Failure ("Assigned string to non-string literal."))
          )
        | MapLiteral(ml) -> raise (Failure ("Assigned string to map literal."))
        | _ -> raise (Failure ("Assigned string to invalid value."))
        )
      (*  CHECK ASSIGN FOR MAP ***********************************************)
      | Map(id) -> (match e with
        Id(o_s) -> let o_dt = List.find (fun dt -> get_dt_name dt = o_s) locals in
          ( (*  Check the assignment of a map to an id *)
          match o_dt with
            Map(s) -> true
            | _ -> raise (Failure "Assigned map to invalid data type.")
          )
        | MapLiteral(ml) -> true
        | _ -> raise (Failure "Asigned map to invalid expr.")
        ) 
      | _ -> raise (Failure ("Not yet implemented check assignment for this dt."))
    (* Basic recursive function for evaluating expressions *)
    in let rec string_of_expr locals = function
      | Literal(l) -> string_of_literal l
      | Assign(s, e) -> 
        (* Before we assign, ensure the assignment is valid *)
        let dt = List.find (fun dt -> get_dt_name dt = s) locals in
          if check_assign locals e dt then
            s ^ " = " ^ string_of_expr locals e
          else
            raise (Failure ("Failed check assign."))
      | MapLiteral(ml) -> into_map ("new Object[]{" ^
          String.concat "," (List.map (fun (d,e) -> string_of_literal d ^ "," ^ string_of_expr locals e) ml) ^ 
          "}")
      | MapGet(id, key) -> id ^ ".get(" ^ string_of_expr locals key ^ ")"
      | MapPut(id, key, v) -> id ^ ".put(" ^ string_of_expr locals key ^ ", " ^ string_of_expr locals v ^ ")"
      | Id(s) -> 
        (* Ensures that the used id is within the current scope *)
        if(List.exists (fun dt -> get_dt_name dt = s) locals) then
          s
        else
          raise (Failure ("Undeclared variable " ^ s))
    in
    let rec string_of_stmt (output, locals) = function
      Block(string_of_stmts) -> 
        let l = List.fold_left string_of_stmt ("", locals) string_of_stmts 
        in (output ^ "{\n" ^ (fst l) ^ "\n}\n", locals)
      | Print(s) -> (output ^ "System.out.println(" ^ string_of_expr locals s ^ ");\n", locals)
      | Expr(e) -> (output ^ string_of_expr locals e ^ ";\n", locals)
      | Declare(dt) -> 
        let name = get_dt_name dt in
        if List.exists (fun dt -> get_dt_name dt = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          (output ^ string_of_data_type dt ^ " = " ^ default_init dt ^ ";\n", dt :: locals)
      | DeclareAssign(dt, e) -> 
        let name = get_dt_name dt in
        if List.exists (fun dt -> get_dt_name dt = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          if check_assign locals e dt then
            (output ^ string_of_data_type dt ^ " = " ^ string_of_expr locals e ^ ";\n", dt :: locals)
          else
            raise (Failure ("Failed check assign on declare assign."))


    in "\npublic static void " ^
    fdecl.fname ^ "(" ^ String.concat ", " (List.map string_of_data_type fdecl.formals) ^ ")"
      ^ fst (string_of_stmt ("", []) (Block fdecl.body)) ^ "\n"
  in let env = { 
      function_index = [];
      global_index = []
    }
  (* The next line is the heart of it ans is where this all really starts *)
  in String.concat "\n" (List.map (translate_helper env) functions)



