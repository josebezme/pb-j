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
      | Boolean(id) -> id 
      | Double(id) -> id
      | Long(id) -> id

    (* This is a utility method for turning a map literal into a valid java expr
        The method it's self is in the backend code. *)
    in let into_map str = 
      "plt.pbj.util.MapUtil.toMap(" ^ str ^ ")"

    (* Returns the default java initialization for a data type. *)
    in let rec default_init = function
      String(id) -> "\"\""
      | Map(id) -> "new HashMap<Object, Object>()"
      | Long(id) -> "0"
      | Double(id) -> "0"
      | Boolean(id) -> "false"
      | _ -> raise (Failure ("initialization not implemented yet."))

    (* Returns the java declaration of a datatype *)
    in let rec string_of_data_type = function
      String(id) -> "String " ^ id
      | Map(id) -> "Map<Object, Object> " ^ id
      | Array(id) -> "List<Object> " ^ id
      | Boolean(id) -> "Boolean " ^ id
      | Long(id) -> "Long " ^ id
      | Double(id) -> "Double " ^ id

    (* Turns a literal object into a java expr *)
    in let rec string_of_literal = function
      StringLiteral(s) -> "\"" ^ s ^ "\""
      | DubLiteral(s) -> s
      | LongLiteral(s) -> s
      | BooleanLiteral(s) -> string_of_bool s

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
        | Concat(e1, e2) -> true
        | MapLiteral(ml) -> raise (Failure ("Assigned string to map literal."))
        | _ -> raise (Failure ("Assigned string to invalid expression."))
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
      (* CHECK ASSIGN FOR LONG ***********************************************)
      | Long(id) -> (match e with
        Id(s) -> let dt = List.find (fun dt -> get_dt_name dt = s) locals in
          (match dt with
            Long(s) -> true
            | _ -> raise (Failure ("Assigned long to invalid non-long id " ^ s))
          )
        | Literal(l) -> (match l with
            LongLiteral(ll) -> true
            | _ -> raise (Failure "Assigned long to non-long literal")
          )
        | Size(id) -> true
        | _ -> raise (Failure "Assigned long to invalid expression.")
        )
      (* CHECK ASSIGN FOR DOUBLE ********************************************)
      | Double(id) -> (match e with
        Id(s) -> let dt = List.find (fun dt -> get_dt_name dt = s) locals in
          (match dt with
            Double(s) -> true
            | _ -> raise (Failure ("Assigned double to non-double id " ^ s))
          )
        | Literal(l) -> (match l with
            DubLiteral(dl) -> true
            | LongLiteral(ll) -> true
            | _ -> raise (Failure "Assigned double to invalid literal.")
          )
        | _ -> raise (Failure "Assigned double to invalid expression.")
        )
      (* CHECK ASSIGN FOR BOOLEAN ******************************************)
      | Boolean(id) -> (match e with
        Id(s) -> let dt = List.find (fun dt -> get_dt_name dt = s) locals in
          (match dt with
            Boolean(bid) -> true
            | _ -> raise (Failure ("Assigned boolean to non-boolean id " ^ s))
          )
        | Literal(l) -> (match l with
            BooleanLiteral(b) -> true
            | _ -> raise (Failure "Assigned boolean to non-boolean literal.")
          )
        | _ -> raise (Failure "Assigned boolean to invalid expression.")
        )
      | _ -> raise (Failure ("Not yet implemented check assignment for this dt."))

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
      | MapGet(id, key) ->
        if is_map locals id then 
          id ^ ".get(" ^ string_of_expr locals key ^ ")"
        else
          raise (Failure (id ^ " is not a valid map type."))
      | MapPut(id, key, v) -> if is_map locals id then
          id ^ ".put(" ^ string_of_expr locals key ^ ", " ^ string_of_expr locals v ^ ")"
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
      | Expr(e) -> (output ^ string_of_expr locals e ^ ";\n", locals)
			| If (p, t, Block([])) -> 
				(output 
            ^ "if(" ^ string_of_expr locals p ^ ") "
            ^ fst (((fun x -> string_of_stmt ("", locals) x) t)), locals)
			| If (p, t, f) -> 
        (output 
            ^ "if(" ^ string_of_expr locals p ^ ") "
            ^ (fst (string_of_stmt ("", locals) t))
            ^ "\n else " ^ (fst (string_of_stmt ("", locals) f)), locals )
      | For (s1, e, e2, b) ->
				(output 
				    ^ "for(" 
						^ (fst (string_of_stmt ("", locals) s1)) 
            ^ string_of_expr locals e ^ "; " 
            ^ string_of_expr locals e2 ^ ") " 
						^ (fst (string_of_stmt ("", locals) b )), locals)
      | While (e, b) ->
				(output 
            ^ "while(" ^ string_of_expr locals e ^ ") " 
            ^ (fst (string_of_stmt ("", locals) b)), locals )
      | DoWhile (b, e) ->
                (output 
					  ^ "do\n" ^ (fst (string_of_stmt ("", locals) b))
            ^ "while(" ^ string_of_expr locals e ^ "); " 
            , locals )
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



