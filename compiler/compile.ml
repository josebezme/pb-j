open Ast

let imports = 
  "import java.util.ArrayList;\n" ^
  "import java.util.Arrays;\n" ^
  "import java.util.List;\n" ^
  "import java.util.Map;\n" ^ 
  "import plt.pbj.util.PBJOp;\n"

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
  in let rec string_of_data_type with_id = function
      String(id) -> "String " ^ (if with_id then id else "")
      | Map(id) -> "Map<Object, Object> " ^ (if with_id then id else "")
      | Array(id) -> "List<Object> " ^ (if with_id then id else "")
      | Boolean(id) -> "Boolean " ^ (if with_id then id else "")
      | Long(id) -> "Long " ^ (if with_id then id else "")
      | Double(id) -> "Double " ^ (if with_id then id else "")
      | Void(id) -> "void " ^ (if with_id then id else "")

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
    "public static final " ^ (string_of_data_type true (fst global)) ^ " = " ^ string_of_literal (snd global) ^ "; \n"  

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
      | Long(id) -> "new Long(0L)"
      | Double(id) -> "new Double(0.0)"
      | Boolean(id) -> "new Boolean(false)"
      | _ -> raise(Failure "No default initialization for this data_type.")

    in let get_dt_from_name name locals =
      try List.find (fun dt -> get_dt_name dt = name) locals
        with Not_found -> raise(Failure ("Variable " ^ name ^ " is undeclared."))

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

    in let is_null = function 
      Null -> true
      | _ -> false

    (* Checks for invalid assignments of data types *)
    in let check_assign locals e dt no_raise =
      let rec match_string_dt = function
      String(s) -> true
      | _ -> false

      in let rec match_array_dt = function
        Array(s) -> true
        | _ -> if no_raise then false
          else raise (Failure ("Assigned array to invalid data type"))

      in let rec match_map_dt = function
        Map(s) -> true
        | _ -> if no_raise then false
          else raise (Failure ("Assigned array to invalid data type"))

      in let rec match_long_dt = function
        Long(s) -> true
        | _ -> if no_raise then false
          else raise (Failure ("Assigned long to invalid data type"))

      in let rec match_double_dt = function
        Double(s) -> true
        | _ -> if no_raise then false
          else raise (Failure ("Assigned double to invalid data type"))

      in let rec match_boolean_dt = function
        Boolean(s) -> true
        | _ -> if no_raise then false
          else raise (Failure ("Assigned boolean to invalid data type"))

      in let rec match_data_type match_func check_func dt = function
        FunctionCall(id,e) -> match_func (get_func_dt id functions)
        | MapPut(id, key, e) -> check_func e dt 
        | ArrayPut(id, idx, e) -> check_func e dt 
        | Assign(id, e) -> check_func e dt 
        | JamSpread(f, sp) -> (match (f, sp) with 
          (FunctionCall(id, e), Spread(FunctionCall(id2, e2)) ) -> 
               match_func (get_func_dt id functions) && match_func (get_func_dt id2 functions)
          | (NoExpr, Spread(FunctionCall(fid, fe))) ->
            (match dt with
              Array(id) -> true
              | _ -> if no_raise then false else raise(Failure("Assigned jam to invalid type."))
            )
          | _ -> raise (Failure ("Improper Jam/Spread.")))
        | Spread(f)        -> (match f with
              FunctionCall(id, e) -> match_func (get_func_dt id functions)
            | _ -> raise (Failure ("Improper Spread.")))
        | NoExpr -> if no_raise then false else raise(Failure("Assigned " ^ get_dt_name dt ^ " "))

      in let rec check_assign_helper e dt = match dt with
        (*  CHECK ASSIGN FOR STRING ***********************************************)
        String(es) -> (match e with
          (* if it's an id get the data type from locals list *)
          Id(id) -> match_string_dt (get_dt_from_name id locals)
          | Literal(l) -> 
            ( (* Check the assignment of a string to an id *)
            match l with 
              StringLiteral(sl) -> true
              | _ ->  if no_raise then false else raise (Failure ("Assigned string to non-string literal."))
            )
          | Concat(e1, e2) -> true
          | MapGet(id, key) -> true
          | ArrayGet(id, idx) -> true 
          | Null -> true
          | StmtExpr(e) -> match_data_type match_string_dt check_assign_helper dt e 
          | _ ->  if no_raise then false else raise (Failure ("Assigned string to invalid expression."))
          )
        (* CHECK ASSIGN FOR ARRAY **********************************************)
        | Array(id) -> (match e with
          Id(id) -> match_array_dt (get_dt_from_name id locals)
          | ArrayLiteral(a) -> true
          | MapValues(id) -> true
          | MapKeys(id) -> true
          | MapGet(id, key) -> true
          | ArrayGet(id, idx) -> true 
          | StmtExpr(e) -> match_data_type match_array_dt check_assign_helper dt e
          | _ -> if no_raise then false else raise (Failure ("Assigned array to invalid expression."))
          )
        (*  CHECK ASSIGN FOR MAP ***********************************************)
        | Map(id) -> (match e with
          Id(id) -> match_map_dt (get_dt_from_name id locals)
          | MapLiteral(ml) -> true
          | MapGet(id, key) -> true
          | ArrayGet(id, idx) -> true
          | StmtExpr(e) -> match_data_type match_map_dt check_assign_helper dt e
          | _ -> if no_raise then false else raise (Failure "Asigned map to invalid expr.")
          ) 
        (* CHECK ASSIGN FOR LONG ***********************************************)
        | Long(id) -> (match e with
          Id(id) -> match_long_dt (get_dt_from_name id locals)
          | Literal(l) -> (match l with
              LongLiteral(ll) -> true
              | _ -> if no_raise then false else raise (Failure "Assigned long to non-long literal")
            )
          | Binop (e1, o, e2) -> check_assign_helper e1 dt
          | Size(id) -> true
          | MapGet(id, key) -> true
          | ArrayGet(id, idx) -> true 
          | StmtExpr(e) -> match_data_type match_long_dt check_assign_helper dt e
          | _ -> if no_raise then false else raise (Failure "Assigned long to invalid expression.")
          )
        (* CHECK ASSIGN FOR DOUBLE ********************************************)
        | Double(id) -> (match e with
          Id(id) -> match_double_dt (get_dt_from_name id locals) 
          | Literal(l) -> (match l with
              DubLiteral(dl) -> true
              | LongLiteral(ll) -> true
              | _ ->  if no_raise then false else raise (Failure "Assigned double to invalid literal.")
            )
          | MapGet(id, key) -> true
          | ArrayGet(id, idx) -> true 
          | Binop (e1, o, e2) -> check_assign_helper e1 dt
          | StmtExpr(e) -> match_data_type match_double_dt check_assign_helper dt e
          | _ -> if no_raise then false else raise (Failure "Assigned double to invalid expression.")
          )
        (* CHECK ASSIGN FOR BOOLEAN ******************************************)
        | Boolean(id) -> (match e with
          Id(id) -> match_boolean_dt (get_dt_from_name id locals)
          | Literal(l) -> (match l with
              BooleanLiteral(b) -> true
              | _ -> if no_raise then false else raise (Failure "Assigned boolean to non-boolean literal.")
            )
          | MapGet(id, key) -> true
          | ArrayGet(id, idx) -> true 
          | Binop (e1, o, e2) -> (match o with
            Seq -> true
            | Peq -> true
            | Greater -> true
            | Geq -> true
            | Less -> true
            | Leq -> true
            | And -> true 
            | Or -> true
            | Neq -> true
            | _ -> false
            )
          | StmtExpr(e) -> match_data_type match_boolean_dt check_assign_helper dt e
          | _ -> if no_raise then false else raise (Failure "Assigned double to invalid expression.")
          )
        | _ -> if no_raise then false else raise (Failure "Invalid assignment")
      in check_assign_helper e dt

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

		  in let rec match_args_dt id e functions locals=
        if does_func_exist id functions then
            (match functions with 
                [] -> raise(Failure ("Failed to find func with params" ^ id ))
                | hd :: tl ->
                    let rec match_formals fli eli = (match (fli, eli) with
                        | ([], []) -> true
                        | (f::fl, e::el) -> if check_assign locals e f true then 
                            match_formals fl el 
                            else false
                       | (_, _) -> false
                )in
                    if (get_dt_name(hd.fname) = id && (match_formals hd.formals e)) then
                        true
                    else
                        match_args_dt id e tl locals
      ) else 
        raise(Failure("Function " ^ id ^ " does not exist with those parameters."))
				
    (* Basic recursive function for evaluating expressions *)
    in let rec string_of_stmt_expr locals = function
      Assign(s, e) ->   
        (* Before we assign, ensure the assignment is valid *)
        let dt = List.find (fun dt -> get_dt_name dt = s) locals in
        s ^ "=" ^ "(" ^ (string_of_data_type false dt) ^ ")" ^ string_of_assignment locals dt e
          
      | ArrayPut(id, idx, e) -> 
        if check_array_index locals idx then
                     "plt.pbj.util.ArrayUtil.set(" ^ id ^ ","
                     ^ string_of_expr locals idx ^ ", " 
                     ^ string_of_expr locals e ^ ")"
        else
          raise (Failure "Should have failed before here.")
      | MapPut(id, key, v) -> if is_map locals id then
          id ^ ".put(" ^ string_of_expr locals key ^ ", " ^ string_of_expr locals v ^ ")"
        else
          raise (Failure (id ^ " is not a valid map type."))
      | FunctionCall(s,e) -> if match_args_dt s e functions locals then 
				s ^ "("  ^  String.concat "," (List.map (string_of_expr locals) e) ^ ")"
				else raise (Failure ("failed earlier"))
      | NoExpr -> ""
      | JamSpread(f, sp)   -> 
				(* jam: jadd(@) spread: add(@myList); *)
        (* create a map from slave to job where job is*)
        (*pass the actuals*)
				(let print_acts list locals = (
              (if List.length list > 0 then "(Object)" else "")
            ^ (String.concat ", (Object)" (List.map (string_of_expr locals) list))
            )
				in let p_r_helper x f locals = (match x with
             | At(_)    -> "PBJOp.jam( \"" ^ (fst(f)) ^ "\", new Object[]{" 
						  ^ (print_acts (snd(f)) locals) ^ "})"
             | l        -> string_of_expr locals l)
				in let print_acts_returned actlist f locals = (
             (if List.length actlist > 0 then "(Object)" else "")
            ^ (String.concat ", (Object)" (List.map (fun x -> (p_r_helper x f locals)) actlist))
            )
        in (match (f, sp) with
				(*Spread(FunctionCall(fid, fe))*)
          (FunctionCall(jid, jargs), Spread(FunctionCall(fid, fe))) ->
					  (*Object[] *)
						jid ^ "( " ^ (print_acts_returned jargs (fid, fe) locals) ^ ")"
          | (NoExpr, Spread(FunctionCall(fid, args))) ->
            "PBJOp.jam(\"" ^ fid ^ "\", new Object[]{ " ^ (print_acts args locals) ^ "})"
					| (_,_) -> raise (Failure("improper Jam Spread 2."))))
      | Spread(f)          -> 
				(let print_acts list locals = (
              (if List.length list > 0 then "(Object)" else "")
            ^ (String.concat ", (Object)" (List.map (string_of_expr locals) list)))
        in (match f with
          FunctionCall(id, args) ->
                        "PBJOp.spread(\"" ^ id ^ "\", new Object[]{ " ^ (print_acts args locals) ^ "})"
          | _ -> raise (Failure("Spread on non-function."))))

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
    	    (* All datatypes are java objects so expression can be any type for .equals() *)
    	    else if o = Seq then true
    	    (* Expression must be the same type for == *)
    	    else if o = Peq then
    	      (check_assign locals e1 dt_long true && check_assign locals e2 dt_long true) ||
    	      (check_assign locals e1 dt_doub true && check_assign locals e2 dt_doub true) ||
    	      (check_assign locals e1 dt_str true && check_assign locals e2 dt_str true) ||
    	      (check_assign locals e1 dt_bool true && check_assign locals e2 dt_bool true) ||
    	      (check_assign locals e1 dt_array true && check_assign locals e2 dt_array true) ||
    	      (check_assign locals e1 dt_map true && check_assign locals e2 dt_map true) ||
            (is_null e1) || (is_null e2)
                (* Expressions must be long or double for arith and comp ops *)
    	    else 
    	      (check_assign locals e1 dt_long true || check_assign locals e1 dt_doub true) 
    		&& (check_assign locals e2 dt_long true || check_assign locals e2 dt_doub true)
    	  in let op_string =   
    	    (match o with
    	      Add -> "+"
    	    | Sub -> "-"
    	    | Mult -> "*"
    	    | Div -> "/"
    	    | Mod -> "%"
    	    | Peq -> "=="
    	    | Greater -> ">"
    	    | Geq -> ">="
    	    | Less -> "<"
    	    | Leq -> "<="
    	    | And -> "&&"
    	    | Or -> "||"
          | _ -> "") in
    	  if check_binop_type locals then (match o with
          Seq -> "(" ^ string_of_expr locals e1 ^ ".equals(" ^ string_of_expr locals e2 ^ "))"
          | Neq -> "(!" ^ string_of_expr locals e1 ^ ".equals(" ^ string_of_expr locals e2 ^ "))"
          | Mod -> "(Long.valueOf(" ^ string_of_expr locals e1 ^ op_string ^ string_of_expr locals e2 ^ "))"
          | _ -> "(" ^ string_of_expr locals e1 ^ op_string ^ string_of_expr locals e2 ^ ")"
          )
    	  else 
    	    if (o = And || o = Or) then raise (Failure ("Invalid Type for operation " ^ op_string ^ ": Both expressions must be type Boolean"))
    	    else if o = Peq then raise (Failure ("Invalid Type for operation " ^ op_string ^ ": Both expressions must be the same type"))
    	    else raise (Failure ("Invalid Type for operation " ^ op_string ^ ": Both expressions must be type Long or Double"))
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
          "new Long(" ^ id ^ ".size())"
        else
          raise (Failure (id ^ " is not a valid map or array."))
      | Concat(e1, e2) ->
        (* Start it off with an empty string so java knows to concat any numeric values vs addition. *)
        "(\"\" + " ^ string_of_expr locals e1 ^ " + " ^ string_of_expr locals e2 ^ ")" 
			| At(e)               ->  "new Spreadable( " ^ string_of_expr locals e ^ ")"
      | Id(s) -> 
        (* Ensures that the used id is within the current scope *)
        if(List.exists (fun dt -> get_dt_name dt = s) locals) then
          s
         else
        if (check_globals s globals) then
          s
        else
          raise (Failure ("Undeclared variable " ^ s))
    and string_of_assignment locals dt e =
      if check_assign locals e dt false then
         (match dt with 
          Long(s) -> "Long.valueOf(\"\" + (" ^ string_of_expr locals e ^ "))" 
          | Double(s) -> "Double.valueOf(\"\" + (" ^ string_of_expr locals e ^ "))"
          | String(s) -> "\"\" + (" ^ string_of_expr locals e ^ ")"
          | _ -> string_of_expr locals e
        )
      else
        raise (Failure ("Failed check assign."))
    
    in let check_valid_for_stmt = function
     ExprAsStmt(stmt_of_expr) -> true
     | DeclareAssign(dt, e) -> true
     | Declare(dt) -> true 
     | NoStmt -> true
     | _ -> false
     
    in let rec string_of_stmt (output, locals) = function
      Block(string_of_stmts) -> 
        let l = List.fold_left string_of_stmt ("", locals) string_of_stmts 
        in (output ^ "{\n" ^ (fst l) ^ "}\n", locals)
      | Print(s) -> (output ^ "System.out.println(" ^ string_of_expr locals s ^ ");\n", locals)
			| If (p, t, Block([])) -> 
				(output 
            ^ "if(" ^ string_of_expr locals p ^ ") "
            ^ fst (((fun x -> string_of_stmt ("", locals) x) t)), locals)
			| If (p, t, f) -> 
        (output 
            ^ "if(" ^ string_of_expr locals p ^ ") "
            ^ (fst (string_of_stmt ("", locals) t))
            ^ "\n else " ^ (fst (string_of_stmt ("", locals) f)), locals )
      | For (s1, e, se, b) ->
        let output_pair = (string_of_stmt ("", locals) s1)
        in let init_locals = (snd output_pair)
        in if (check_valid_for_stmt s1) then
         if (check_assign init_locals e (Boolean("")) true) then
            (output 
      				    ^ "for(" 
      						^ (fst output_pair) 
                  ^ string_of_expr init_locals e ^ "; " 
                  ^ string_of_stmt_expr init_locals se ^ ") " 
      						^ (fst (string_of_stmt ("", init_locals) b )), locals)
          else raise (Failure ("For condition must return a boolean expression.")) 
        else raise (Failure ("For initialization must be a valid statment."))  
      | While (e, b) ->
				(output 
            ^ "while(" ^ string_of_expr locals e ^ ") " 
            ^ (fst (string_of_stmt ("", locals) b)), locals )
      | DoWhile (b, e) ->
                (output 
					  ^ "do\n" ^ (fst (string_of_stmt ("", locals) b))
            ^ "while(" ^ string_of_expr locals e ^ "); " 
            , locals )
      | Return(e) -> 
        if (check_assign locals e fdecl.fname true || is_null e) then
        ( output ^ "return " ^ "("^ string_of_data_type false fdecl.fname ^ ")" ^ string_of_expr locals e ^ ";\n", locals)
        else
          raise (Failure ("Invalid return expression for function: " ^ (get_dt_name fdecl.fname)))
      | ExprAsStmt(e) -> (output ^ string_of_stmt_expr locals e ^ ";\n", locals)
      | Declare(dt) -> 
        let name = get_dt_name dt in
        if List.exists (fun dt -> get_dt_name dt = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          (output ^ (string_of_data_type true dt) ^ " = " ^ default_init dt ^ ";\n", dt :: locals)
      | DeclareAssign(dt, e) -> 
        let name = get_dt_name dt in
        if List.exists (fun dt -> get_dt_name dt = name) locals then
          raise (Failure ("Variable " ^ name ^ " has already been declared."))
        else
          if check_assign locals e dt false then
            (output ^ (string_of_data_type true dt) ^ " = " ^ 
              "(" ^ (string_of_data_type false dt) ^ ")" ^ 
              (string_of_assignment locals dt e) ^ ";\n", dt :: locals)
          else
            raise (Failure ("Failed check assign on declare assign."))
      | NoStmt -> (output ^ ";", locals)

    in "public static " ^
    (string_of_data_type true fdecl.fname) ^ "(" ^ String.concat ", " (List.map (string_of_data_type true) fdecl.formals) ^ ")"
      ^ fst (string_of_stmt ("", fdecl.formals) (Block fdecl.body))
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
    "package plt.pbj;\n" ^ imports ^ "public class PBJRunner {\n" ^
    String.concat "" (List.map (translate_globals) globals) ^
    String.concat "" (List.map (translate_helper) functions) ^
    "}\n"
  else
    raise(Failure("Functions had invalid returns."))
