open Ast

type env = {
    global_index   : string list;
  }

let translate (globals, functions) =
  
  (* This just returns the string name for a data type *)
    let rec get_dt_name = function
      String(id) -> id
      | Map(id) -> id
      | Array(id) -> id
      | Boolean(id) -> id
      | Double(id) -> id
      | Long(id) -> id
      | Void(id) -> id

  (* Translate a function with given env *)
  in let translate_helper fdecl =

    (* This is a utility method for turning a map literal into a valid java expr
        The method it's self is in the backend code. *)
    let into_map str = 
      "plt.pbj.util.MapUtil.toMap(" ^ str ^ ")"

    (* Returns the default java initialization for a data type. *)
    in let rec default_init = function
      String(id) -> "\"\""
      | Array(id) -> "new ArrayList<Object>()"
      | Map(id) -> "new HashMap<Object, Object>()"
      | Long(id) -> "0L"
      | Double(id) -> "0.0"
      | Boolean(id) -> "false"
      | _ -> raise(Failure "No default initialization for this data_type.")

    (* Returns the java declaration of a datatype *)
    in let rec string_of_data_type = function
      String(id) -> "String " ^ id
      | Map(id) -> "Map<Object, Object> " ^ id
      | Array(id) -> "List<Object> " ^ id
      | Boolean(id) -> "Boolean " ^ id
      | Long(id) -> "Long " ^ id
      | Double(id) -> "Double " ^ id
      | Void(id) -> "void " ^ id

    (* Turns a literal object into a java expr *)
    in let rec string_of_literal = function
      StringLiteral(s) -> "\"" ^ s ^ "\""
      | DubLiteral(s) -> "new Double(" ^ s ^ ")"
      | LongLiteral(s) -> "new Long(" ^ s ^ ")"
      | BooleanLiteral(s) -> string_of_bool s

    in let get_dt_from_name name locals =
      List.find (fun dt -> get_dt_name dt = name) locals

    in let rec check_array_index locals = function
      Id(id) -> (match (get_dt_from_name id locals) with
          Long(id) -> true
          | _ -> raise (Failure ("Variable " ^ id ^ " is not valid index for array."))
        )
      | Literal(l) -> (match l with 
          LongLiteral(s) -> true
          | _ -> raise (Failure ("Used invalid data type for array index."))
        )
      | MapGet(id, key) -> true
      | ArrayGet(id, idx) -> true
      | StmtExpr(e) -> (match e with
          ArrayPut(id, idx, e) -> check_array_index locals e
          | _ -> raise (Failure ("Used invalid expression for array index."))
        )
      | Size(id) -> true
      | _ -> raise (Failure ("Used invalid expression for array index."))

    in let rec does_func_exist id = function
      [] -> false
      | hd :: tl -> 
        if get_dt_name(hd.fname) = id then
          true
        else
          does_func_exist id tl

    in let rec get_func_dt id functions =
      if does_func_exist id functions then
        (match functions with 
          [] -> raise(Failure ("Failed to find func" ^ id))
          | hd :: tl ->
            if get_dt_name(hd.fname) = id then
              hd.fname
            else
              get_func_dt id tl
      ) else 
        raise(Failure("Function " ^ id ^ " does not exist."))

    in let rec match_string_dt = function
      String(s) -> true
      | _ -> raise (Failure ("Assigned string to invalid data type"))

    in let rec match_array_dt = function
      Array(s) -> true
      | _ -> raise (Failure ("Assigned array to invalid data type"))

    in let rec match_map_dt = function
      Map(s) -> true
      | _ -> raise (Failure ("Assigned array to invalid data type"))

    in let rec match_long_dt = function
      Long(s) -> true
      | _ -> raise (Failure ("Assigned long to invalid data type"))

    in let rec match_double_dt = function
      Double(s) -> true
      | _ -> raise (Failure ("Assigned double to invalid data type"))

    in let rec match_boolean_dt = function
      Boolean(s) -> true
      | _ -> raise (Failure ("Assigned boolean to invalid data type"))

    in let rec match_data_type locals match_func check_func dt = function
      FunctionCall(id,e) -> match_func (get_func_dt id functions)
      | MapPut(id, key, e) -> check_func locals e dt
      | ArrayPut(id, idx, e) -> check_func locals e dt
      | Assign(id, e) -> check_func locals e dt

    (* Checks for invalid assignments of data types *)
    in let rec check_assign locals e dt = match dt with
      (*  CHECK ASSIGN FOR STRING ***********************************************)
      String(es) -> (match e with
        (* if it's an id get the data type from locals list *)
        Id(id) -> match_string_dt (get_dt_from_name id locals)
        | Literal(l) -> 
          ( (* Check the assignment of a string to an id *)
          match l with 
            StringLiteral(sl) -> true
            | _ -> raise (Failure ("Assigned string to non-string literal."))
          )
        | Concat(e1, e2) -> true
        | MapLiteral(ml) -> raise (Failure ("Assigned string to map literal."))
        | StmtExpr(e) -> match_data_type locals match_string_dt check_assign dt e
        | _ -> raise (Failure ("Assigned string to invalid expression."))
        )
      (* CHECK ASSIGN FOR ARRAY **********************************************)
      | Array(id) -> (match e with
        Id(id) -> match_array_dt (get_dt_from_name id locals)
        | ArrayLiteral(a) -> true
        | MapValues(id) -> true
        | MapKeys(id) -> true
        | StmtExpr(e) -> match_data_type locals match_array_dt check_assign dt e
        | _ -> raise (Failure ("Assigned array to invalid expression."))
        )
      (*  CHECK ASSIGN FOR MAP ***********************************************)
      | Map(id) -> (match e with
        Id(id) -> match_map_dt (get_dt_from_name id locals)
        | MapLiteral(ml) -> true
        | MapGet(id, key) -> true
        | ArrayGet(id, idx) -> true
        | StmtExpr(e) -> match_data_type locals match_map_dt check_assign dt e
        | _ -> raise (Failure "Asigned map to invalid expr.")
        ) 
      (* CHECK ASSIGN FOR LONG ***********************************************)
      | Long(id) -> (match e with
        Id(id) -> (match (get_dt_from_name id locals) with
            Long(s) -> true
            | _ -> raise (Failure ("Assigned long to invalid non-long id " ^ id))
          )
        | Literal(l) -> (match l with
            LongLiteral(ll) -> true
            | _ -> raise (Failure "Assigned long to non-long literal")
          )
        | Size(id) -> true
        | StmtExpr(e) -> match_data_type locals match_long_dt check_assign dt e
        | _ -> raise (Failure "Assigned long to invalid expression.")
        )
      (* CHECK ASSIGN FOR DOUBLE ********************************************)
      | Double(id) -> (match e with
        Id(id) -> match_double_dt (get_dt_from_name id locals) 
        | Literal(l) -> (match l with
            DubLiteral(dl) -> true
            | LongLiteral(ll) -> true
            | _ -> raise (Failure "Assigned double to invalid literal.")
          )
        | StmtExpr(e) -> match_data_type locals match_double_dt check_assign dt e
        | _ -> raise (Failure "Assigned double to invalid expression.")
        )
      (* CHECK ASSIGN FOR BOOLEAN ******************************************)
      | Boolean(id) -> (match e with
        Id(id) -> match_boolean_dt (get_dt_from_name id locals)
        | Literal(l) -> (match l with
            BooleanLiteral(b) -> true
            | _ -> raise (Failure "Assigned boolean to non-boolean literal.")
          )
        | StmtExpr(e) -> match_data_type locals match_boolean_dt check_assign dt e
        | _ -> raise (Failure "Assigned boolean to invalid expression.")
        )
      | _ -> raise (Failure "Invalid assignment")

    in let is_map locals id =
      let rec is_map_helper = function
        Map(id) -> true
        | _ -> false
      in List.exists (fun dt -> get_dt_name dt = id && is_map_helper dt) locals

    in let is_array locals id =
      let rec is_array_helper = function
        Array(id) -> true
        | _ -> false
      in List.exists (fun dt -> get_dt_name dt = id && is_array_helper dt) locals

    (* Basic recursive function for evaluating expressions *)
    in let rec string_of_stmt_expr locals = function
      Assign(s, e) ->   
        (* Before we assign, ensure the assignment is valid *)
        let dt = List.find (fun dt -> get_dt_name dt = s) locals in
          if check_assign locals e dt then
            s ^ " = " ^ string_of_expr locals e
          else
            raise (Failure ("Failed check assign."))
      | ArrayPut(id, idx, e) -> 
        if check_array_index locals idx then
                     id
                     ^ ".set(" 
                     ^ string_of_expr locals idx 
                     ^ ".intValue()"
                     ^ ", " 
                     ^ string_of_expr locals e 
                     ^ ")"
        else
          raise (Failure "Should have failed before here.")
      | MapPut(id, key, v) -> if is_map locals id then
          id ^ ".put(" ^ string_of_expr locals key ^ ", " ^ string_of_expr locals v ^ ")"
        else
          raise (Failure (id ^ " is not a valid map type."))
      | FunctionCall(s,e) -> s ^ "("  ^  String.concat "," (List.map (string_of_expr locals) e) ^ ")" 

    and string_of_expr locals = function
      StmtExpr(e) -> string_of_stmt_expr locals e
      | Literal(l) -> string_of_literal l
      | MapLiteral(ml) -> into_map ("new Object[]{" ^
          String.concat "," (List.map (fun (d,e) -> string_of_literal d ^ "," ^ string_of_expr locals e) ml) ^ 
          "}")
      | ArrayLiteral(a) -> 
          let rec array_expr locals array = match array with 
                  []        -> []
                | e::a      -> string_of_expr locals e :: array_expr locals a
          in "new ArrayList<Object> (Arrays.asList(" ^ (String.concat ", " (List.rev (array_expr locals a))) ^ "))"
      | Null                -> "null"
      | ArrayGet(id, idx)      -> 
        if check_array_index locals idx then
          id ^ ".get(" ^ string_of_expr locals idx ^ ".intValue())"(**)
        else
          raise (Failure "Should have failed before here.")
      | MapGet(id, key) ->
        if is_map locals id then 
          id ^ ".get(" ^ string_of_expr locals key ^ ")"
        else
          raise (Failure (id ^ " is not a valid map type."))
      | MapKeys(id) ->  if is_map locals id then
          "new ArrayList<Object>(" ^ id ^ ".keySet())"
        else
          raise (Failure (id ^ " is not a valid map type."))
      | MapValues(id) -> if is_map locals id then
          "new ArrayList<Object>(" ^ id ^ ".values()"
        else
          raise (Failure (id ^ " is not a valid map type."))
      | Size(id) -> if (is_map locals id || is_array locals id) then
          id ^ ".size()"
        else
          raise (Failure (id ^ " is not a valid map or array."))
      | Concat(e1, e2) ->
        (* Start it off with an empty string so java knows to concat any numeric values vs addition. *)
        "(\"\" + " ^ string_of_expr locals e1 ^ " + " ^ string_of_expr locals e2 ^ ")" 
      | Id(s) -> 
        (* Ensures that the used id is within the current scope *)
        if(List.exists (fun dt -> get_dt_name dt = s) locals) then
          s
        else
          raise (Failure ("Undeclared variable " ^ s))

    in let rec string_of_stmt (output, locals) = function
      Block(string_of_stmts) -> 
        let l = List.fold_left string_of_stmt ("", locals) string_of_stmts 
        in (output ^ "{\n" ^ (fst l) ^ "\n}\n", locals)
      | Print(s) -> (output ^ "System.out.println(" ^ string_of_expr locals s ^ ");\n", locals)
      | Return(e) -> (output ^ "return " ^ string_of_expr locals e ^ ";\n", locals)
      | ExprAsStmt(e) -> (output ^ string_of_stmt_expr locals e ^ ";\n", locals)
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

    in "\npublic static " ^
    (string_of_data_type fdecl.fname) ^ "(" ^ String.concat ", " (List.map string_of_data_type fdecl.formals) ^ ")"
      ^ fst (string_of_stmt ("", []) (Block fdecl.body)) ^ "\n"
  in let func_is_void = function
    Void(s) -> true
    | _ -> false

  in let rec check_all_are_true = function
    [] -> true
    | hd :: tl -> 
      if hd then
        check_all_are_true tl
      else 
        false

  in let check_return_statements func =  
    let rec check_return_helper last_stmt is_outer = function
      [] -> (match last_stmt with
          Return(e) -> true
          | _ -> if ((not is_outer) || func_is_void func.fname) then true
            else false
        )
      | hd :: tl -> (match hd with
          Block(stmts) -> 
            if check_return_helper hd false stmts then 
              check_return_helper hd is_outer tl
            else
              false
          | _ -> check_return_helper hd is_outer tl
        )
  in check_return_helper (Block []) true (func.body)

  (* The next line is the heart of it ans is where this all really starts *)
  in if check_all_are_true (List.map check_return_statements functions) then 
    String.concat "\n" (List.map (translate_helper) functions)
  else
    raise(Failure("Functions had invalid returns."))



