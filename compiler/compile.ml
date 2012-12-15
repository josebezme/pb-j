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

  in let rec check_globals id globals = match globals with
    [] -> false
    | hd :: tl -> 
        if get_dt_name(fst hd) = id then
          true
        else
          check_globals id tl

    (* Translate a global *)
  in let translate_globals global =
    "public static final " ^ string_of_data_type (fst global) ^ " = " ^ string_of_literal (snd global) ^ "; \n"  

  (* Translate a function with given env *)
  in let translate_helper env fdecl =

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
      | _ -> false

    (* Checks for invalid assignments of data types *)
    in let rec check_assign locals e dt bi = match dt with
      (*  CHECK ASSIGN FOR STRING ***********************************************)
      String(es) -> (match e with
        (* if it's an id get the data type from locals list *)
        Id(id) -> if match_string_dt (get_dt_from_name id locals) then true
	    else if bi then false else raise (Failure ("Assigned string to invalid data type"))
        | Literal(l) -> 
          ( (* Check the assignment of a string to an id *)
          match l with 
            StringLiteral(sl) -> true
            | _ ->  if bi then false else raise (Failure ("Assigned string to non-string literal."))
          )
        | Concat(e1, e2) -> true
        | MapLiteral(ml) ->  if bi then false else raise (Failure ("Assigned string to map literal."))
        | StmtExpr(e) -> (match e with
            FunctionCall(id,e) -> if match_string_dt (get_func_dt id functions) then true
	                              else if bi then false else raise (Failure ("Assigned string to invalid data type"))
          | MapPut(id, key, e) -> check_assign locals e dt bi
          | ArrayPut(id, idx, e) -> check_assign locals e dt bi
          | Assign(id, e) -> check_assign locals e dt bi
          )
        | _ ->  if bi then false else raise (Failure ("Assigned string to invalid expression."))
        )
      (* CHECK ASSIGN FOR ARRAY **********************************************)
      | Array(id) -> (match e with
        Id(id) -> (match (get_dt_from_name id locals) with
            Array(id) -> true
            | _ -> if bi then false else raise (Failure ("Assigned array to non-array type: " ^ id))
          )
        | ArrayLiteral(a) -> true
        | MapValues(id) -> true
        | MapKeys(id) -> true
        | _ -> if bi then false else raise (Failure ("Assigned array to invalid expression."))
        )
      (*  CHECK ASSIGN FOR MAP ***********************************************)
      | Map(id) -> (match e with
        Id(id) -> (match (get_dt_from_name id locals) with
            Map(s) -> true
            | _ -> if bi then false else raise (Failure "Assigned map to invalid data type.")
          )
        | MapLiteral(ml) -> true
        | _ -> if bi then false else raise (Failure "Asigned map to invalid expr.")
        ) 
      (* CHECK ASSIGN FOR LONG ***********************************************)
      | Long(id) -> (match e with
        Id(id) -> (match (get_dt_from_name id locals) with
            Long(s) -> true
            | _ -> if bi then false else raise (Failure ("Assigned long to invalid non-long id " ^ id))
          )
        | Literal(l) -> (match l with
            LongLiteral(ll) -> true
            | _ -> if bi then false else raise (Failure "Assigned long to non-long literal")
          )
        | Size(id) -> true
        | _ -> if bi then false else raise (Failure "Assigned long to invalid expression.")
        )
      (* CHECK ASSIGN FOR DOUBLE ********************************************)
      | Double(id) -> (match e with
        Id(id) -> (match (get_dt_from_name id locals) with
            Double(s) -> true
            | _ ->  if bi then false else raise (Failure ("Assigned double to non-double id " ^ id))
          )
        | Literal(l) -> (match l with
            DubLiteral(dl) -> true
            | LongLiteral(ll) -> true
            | _ ->  if bi then false else raise (Failure "Assigned double to invalid literal.")
          )
        | _ -> if bi then false else raise (Failure "Assigned double to invalid expression.")
        )
      (* CHECK ASSIGN FOR BOOLEAN ******************************************)
      | Boolean(id) -> (match e with
        Id(id) -> (match (get_dt_from_name id locals) with
            Boolean(bid) -> true
            | _ -> if bi then false else raise (Failure ("Assigned boolean to non-boolean id " ^ id))
          )
        | Literal(l) -> (match l with
            BooleanLiteral(b) -> true
            | _ -> if bi then false else raise (Failure "Assigned boolean to non-boolean literal.")
          )
        | _ -> if bi then false else raise (Failure "Assigned boolean to invalid expression.")
        )
      | _ -> if bi then false else raise (Failure "Invalid assignment")

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
          if check_assign locals e dt false then
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
      | Binop (e1, o, e2) -> 
	  let dt_long = Long("Temp1") in
	  let dt_doub = Double("Temp2") in
	  let dt_str = String("Temp3") in
	  let dt_bool = Boolean("Temp4") in
	  let dt_array = Array("Temp5") in
	  let dt_map = Map("Temp6") in
	  let check_binop_type locals =
	    (* Expressions must be booleans for logical ops *)
	    if (o = And || o = Or) then 
	      check_assign locals e1 dt_bool true && check_assign locals e2 dt_bool true
	    (* First expression must be an object for .equals() *)
	    else if o = Peq then
	      check_assign locals e1 dt_str true || check_assign locals e1 dt_array true || check_assign locals e1 dt_map true
	    (* Expression must be the same type for == *)
	    else if o = Seq then
	      (check_assign locals e1 dt_long true && check_assign locals e2 dt_long true) ||
	      (check_assign locals e1 dt_doub true && check_assign locals e2 dt_doub true) ||
	      (check_assign locals e1 dt_str true && check_assign locals e2 dt_str true) ||
	      (check_assign locals e1 dt_bool true && check_assign locals e2 dt_bool true) ||
	      (check_assign locals e1 dt_array true && check_assign locals e2 dt_array true) ||
	      (check_assign locals e1 dt_map true && check_assign locals e2 dt_map true)
            (* Expressions must be long or double for arith and comp ops *)
	    else 
	      (check_assign locals e1 dt_long true || check_assign locals e1 dt_doub true) 
		&& (check_assign locals e2 dt_long true || check_assign locals e2 dt_doub true)
	  in if check_binop_type locals then
	    let line = string_of_expr locals e1 ^ 
	      (match o with
		Add -> "+"
	      | Sub -> "-"
	      | Mult -> "*"
	      | Div -> "/"
	      | Mod -> "%"
	      | Seq -> "=="
	      | Peq -> ".equals("
	      | Greater -> ">"
	      | Geq -> ">="
	      | Less -> "<"
	      | Leq -> "<="
	      | And -> "&&"
	      | Or -> "||") in
	    if o = Peq then line ^ string_of_expr locals e2 ^ ")"
	    else line ^ string_of_expr locals e2
	  else 
	    if (o = And || o = Or) then raise (Failure ("Invalid Type: Both expressions must be type Boolean"))
		else if o = Seq then raise (Failure ("Invalid Type: Both expressions must be the same type"))
		    else if o = Peq then raise (Failure ("Invalid Type: First expression must be type String, Array, or Map"))
			else raise (Failure ("Invalid Type: Both expressions must be type Long or Double"))
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
        if (check_globals s globals) then
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
          if check_assign locals e dt false then
            (output ^ string_of_data_type dt ^ " = " ^ string_of_expr locals e ^ ";\n", dt :: locals)
          else
            raise (Failure ("Failed check assign on declare assign."))

    in "\npublic static " ^
    (string_of_data_type fdecl.fname) ^ "(" ^ String.concat ", " (List.map string_of_data_type fdecl.formals) ^ ")"
      ^ fst (string_of_stmt ("", []) (Block fdecl.body)) ^ "\n"

  in let env = { 
      global_index = []
    }
   (* The next line is the heart of it ans is where this all really starts *)
  in String.concat "" (List.map (translate_helper env) functions) ^ String.concat "" (List.map (translate_globals) globals) 
