open Ast
open Sast

module StringMap = Map.Make(String)

let check(globals, functions) = 

  let check_dup_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
        [] -> ()
      |	((_,n1) :: (_,n2) :: _) when n1 = n2 ->
        raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in dups (List.sort (fun (_,a) (_,b) -> compare a b) binds)
  in


  (* Collect function declarations for built-in functions: no bodies *)
  let built_in_decls =
    StringMap.add "print" {
      rtyp = Int;
      fname = "print";
      formals = [(String, "x")];
      body = [] } StringMap.empty
  in

  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " is a built-in function"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *)
    in match fd with (* No duplicate functions or redefinitions of built-ins *)
      _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ ->  StringMap.add n fd map
  in

  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions
  in

  (* Return a function from our symbol table *)
  let find_func s =
    try StringMap.find s function_decls
    with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in

  let _ = find_func "main" in (* Ensure "main" is defined *)

  (* Return a variable type from our local symbol table *)
  let type_of_identifier s symbols =
    try StringMap.find s symbols
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in

  (* Raise an exception if the given rvalue type cannot be assigned to
      the given lvalue type *)
  let check_assign lvaluet rvaluet err =
    match lvaluet, rvaluet with
    | Float, Int -> Float
    | _, _ when lvaluet = rvaluet -> lvaluet
    | _, _ -> raise (Failure err)
  in

  let sglobals = 
  (* check global variables declaration *)
  let check_globals globals = 
    (* Make sure no globals duplicate *)
    let global_t = List.map (fun (t, id, e) -> t, id) globals in check_dup_binds "global" global_t;

    let globals' = List.map (fun (t, id, e) -> t, id) globals in
    let global_symbols = List.fold_left (fun m (t, id) -> StringMap.add id t m) StringMap.empty globals' in

    (* disallow function calls in the global scope *)
    let rec check_global_expr = function
    | DefaultValue -> (Void, SDefaultValue)
    | Literal l -> (Int, SLiteral l)
    | BoolLit l -> (Bool, SBoolLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | StringLit l -> (String, SStringLit l)
    | Id var -> let t' = type_of_identifier var global_symbols in (t', SId var)
    | Assign(var, e) as ex ->
      let lt = type_of_identifier var global_symbols
      and (rt, e') = check_global_expr e in
      let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                string_of_typ rt ^ " in " ^ string_of_expr ex
      in
      (check_assign lt rt err, SAssign(var, (rt, e')))

    | Binop(e1, op, e2) as e ->
      let (t1, e1') = check_global_expr e1
      and (t2, e2') = check_global_expr e2 in
      let err = "illegal binary operator " ^
                string_of_typ t1 ^ " " ^ string_of_binop op ^ " " ^
                string_of_typ t2 ^ " in " ^ string_of_expr e
      in
      (* All binary operators require operands of the same type*)
      if t1 = t2 then
        (* Determine expression type based on operator and operand types *)
        let t = match op with
            Add | Sub when t1 = Int -> Int
          | Equal | Neq -> Bool
          | Less when t1 = Int -> Bool
          | And | Or when t1 = Bool -> Bool
          | _ -> raise (Failure err)
        in
        (t, SBinop((t1, e1'), op, (t2, e2')))
      (* Allows for implicit string casting, e.g. 3+"1"="31" *)
      else if op = Add && (t1 = String || t2 = String) then begin
        match t1, t2 with
          | String, t -> begin
            match t with
            | Int | Float -> (String, SBinop((String, e1'), Add, (String, e2')))
            | _ -> raise (Failure err)
          end
          | t, String -> begin
            match t with
            | Int | Float -> (String, SBinop((String, e1'), Add, (String, e2')))
            | _ -> raise (Failure err)
          end
          | _ -> raise (Failure err)
        end
      else raise (Failure err)
    | Unop(op, e) -> begin
      match op with 
      | Not ->
        let (t, e') = check_global_expr e in
        let err = "illegal expression of type " ^ string_of_typ t ^ ", exepected boolean" in
        if t = Bool then (Bool, SUnop(Not, (Bool, e')))
        else raise (Failure err)
      end
    | Call(fname, args) as call -> raise (Failure "illegal call in the global scope")
    in

    let check_global_bind b = let (t, v, e) = b in
      let (t', e') = check_global_expr e in
      if e = DefaultValue then begin
      match t with
      | Int -> t, v, (Int, SLiteral 0)
      | Bool -> t, v, (Bool, SBoolLit(false))
      | Float -> t, v, (Float, SFloatLit(0.0))
      | String -> t, v, (String, SStringLit(""))
      | _ -> raise (Failure "unrecognized type: ")
      end
      else if t = Void then t', v, (t', e')
      else let err = "type mismatch. " ^ v ^ ": " ^ string_of_typ t ^ ", received " ^ string_of_typ t' in
        check_assign t t' err, v, (t', e')
    in
  List.map check_global_bind globals in
  check_globals globals in 


  let check_func func =
    (* Make sure no formals or locals are void or duplicates *)
    check_dup_binds "formal" func.formals;

    (* Build local symbol table of variables for this function *)
    let globals' = List.map (fun (t, id, e) -> t, id) globals in
    let symbols = List.fold_left (fun m (t, id) -> StringMap.add id t m) StringMap.empty (globals' @ func.formals)
    in

    (* let to_string  *)

    (* Return a semantically-checked expression, i.e., with a type *)
    let rec check_expr = function
      | DefaultValue -> (Void, SDefaultValue)
      | Literal l -> (Int, SLiteral l)
      | BoolLit l -> (Bool, SBoolLit l)
      | FloatLit l -> (Float, SFloatLit l)
      | StringLit l -> (String, SStringLit l)
      | Id var -> let t' = type_of_identifier var symbols in (t', SId var)
      | Assign(var, e) as ex ->
        let lt = type_of_identifier var symbols
        and (rt, e') = check_expr e in
        let err = "illegal assignment " ^ string_of_typ lt ^ " = " ^
                  string_of_typ rt ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign(var, (rt, e')))

      | Binop(e1, op, e2) as e ->
        let (t1, e1') = check_expr e1
        and (t2, e2') = check_expr e2 in
        let err = "illegal binary operator " ^
                  string_of_typ t1 ^ " " ^ string_of_binop op ^ " " ^
                  string_of_typ t2 ^ " in " ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t = match op with
              Add | Sub when t1 = Int -> Int
            | Equal | Neq -> Bool
            | Less when t1 = Int -> Bool
            | And | Or when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop((t1, e1'), op, (t2, e2')))
        (* Allows for implicit string casting, e.g. 3+"1"="31" *)
        else if op = Add && (t1 = String || t2 = String) then begin
          match t1, t2 with
            | String, t -> begin
              match t with
              | Int | Float -> (String, SBinop((String, e1'), Add, (String, e2')))
              | _ -> raise (Failure err)
            end
            | t, String -> begin
              match t with
              | Int | Float -> (String, SBinop((String, e1'), Add, (String, e2')))
              | _ -> raise (Failure err)
            end
            | _ -> raise (Failure err)
          end
        else raise (Failure err)
      | Unop(op, e) -> begin
        match op with 
        | Not ->
          let (t, e') = check_expr e in
          let err = "illegal expression of type " ^ string_of_typ t ^ ", exepected boolean" in
          if t = Bool then (Bool, SUnop(Not, (Bool, e')))
          else raise (Failure err)
        end
      | Call(fname, args) as call ->
        let fd = find_func fname in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise (Failure ("expecting " ^ string_of_int param_length ^
                          " arguments in " ^ string_of_expr call))
        else let check_call (ft, _) e =
              let (et, e') = check_expr e in
              let err = "illegal argument found " ^ string_of_typ et ^
                        " expected " ^ string_of_typ ft ^ " in " ^ string_of_expr e
              in (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args
          in (fd.rtyp, SCall(fname, args'))
    in

    let check_bool_expr e =
      let (t, e') = check_expr e in
      match t with
      | Bool -> (t, e')
      |  _ -> raise (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in

    let check_bind b = let (t, v, e) = b in
      let (t', e') = check_expr e in
      if e = DefaultValue then begin
      match t with
      | Int -> t, v, (Int, SLiteral 0)
      | Bool -> t, v, (Bool, SBoolLit(false))
      | Float -> t, v, (Float, SFloatLit(0.0))
      | String -> t, v, (String, SStringLit(""))
      | _ -> raise (Failure "unrecognized type: ")
      end
      else if t = Void then t', v, (t', e')
      else let err = "type mismatch. " ^ v ^ ": " ^ string_of_typ t ^ ", received " ^ string_of_typ t' in
        check_assign t t' err, v, (t', e')
      in


    let rec check_stmt_list = function
        [] -> []
      | Block sl :: sl'  -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl -> check_stmt s :: check_stmt_list sl
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing
        follows any Return statement.  Nested blocks are flattened. *)
        Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e)
      | If(e, st1, st2) ->
        SIf(check_bool_expr e, check_stmt st1, check_stmt st2)
      | Ifd(e, st1) ->
        SIfd(check_bool_expr e, check_stmt st1)
      | While(e, st) ->
        SWhile(check_bool_expr e, check_stmt st)
        (* TODO: uncomment after  fix binding *)
      (* | For(e1, e2, e3, st) ->
        SFor(check_bind e1,check_bool_expr e2, check_expr e3, check_stmt st)   *)
      | Return e ->
        let (t, e') = check_expr e in
        if t = func.rtyp then SReturn (t, e')
        else raise (
            Failure ("return gives " ^ string_of_typ t ^ " expected " ^
                    string_of_typ func.rtyp ^ " in " ^ string_of_expr e))
      (* var x: int *)
      (* var x: int = 2 *)
      (* var x = 2 *)
      | Bind(b) -> let sb = check_bind b in SBind(sb)
      (* | For(e1, e2, e3, st) -> SFor(e1, e2, e3, ) *)
    
    in (* body of check_func *)
    { srtyp = func.rtyp;
      sfname = func.fname;
      sformals = func.formals;
      sbody = check_stmt_list func.body
    }
  in

  (sglobals, List.map check_func functions)
