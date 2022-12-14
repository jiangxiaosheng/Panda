open Ast
open Sast
module StringMap = Map.Make (String)

let check (globals, functions) =
  let check_dup_binds (kind : string) (binds : (typ * string) list) =
    let rec dups = function
      | [] -> ()
      | (_, n1) :: (_, n2) :: _ when n1 = n2 ->
          raise (Failure ("duplicate " ^ kind ^ " " ^ n1))
      | _ :: t -> dups t
    in
    dups (List.sort (fun (_, a) (_, b) -> compare a b) binds)
  in
  (* Collect function declarations for built-in functions: no bodies *)
  let builtins_funcs = [
    {rtyp=Int; fname="print"; formals=[(String, "x")]; body=[]};
    {rtyp=Int; fname="assert"; formals=[(Bool, "c"); (String, "s")]; body=[]};
  ] in
  let built_in_decls =
    List.fold_left (fun m f_decl -> StringMap.add f_decl.fname f_decl m )
      StringMap.empty builtins_funcs
  in
  (* Add function name to symbol table *)
  let add_func map fd =
    let built_in_err = "function " ^ fd.fname ^ " is a built-in function"
    and dup_err = "duplicate function " ^ fd.fname
    and make_err er = raise (Failure er)
    and n = fd.fname (* Name of the function *) in
    match fd with
    (* No duplicate functions or redefinitions of built-ins *)
    | _ when StringMap.mem n built_in_decls -> make_err built_in_err
    | _ when StringMap.mem n map -> make_err dup_err
    | _ -> StringMap.add n fd map
  in
  (* Collect all function names into one symbol table *)
  let function_decls = List.fold_left add_func built_in_decls functions in
  (* Return a function from our symbol table *)
  let find_func s local_funcs =
    try StringMap.find s function_decls
  with Not_found -> try Hashtbl.find local_funcs s 
  with Not_found -> raise (Failure ("unrecognized function " ^ s))
  in
  let _ = find_func "main" (Hashtbl.create 0) in
  (* Ensure "main" is defined *)

  (* Return a variable type from our local symbol table *)
  let type_of_identifier s symbols =
    try Hashtbl.find symbols s
    with Not_found -> raise (Failure ("undeclared identifier " ^ s))
  in
  let add_symbols id t sym =
    try
      let _ = Hashtbl.find sym id in
      raise (Failure ("symbol " ^ id ^ " is duplicate"))
    with Not_found -> Hashtbl.add sym id t
  in
  (* Raise an exception if the given rvalue type cannot be assigned to the
     given lvalue type *)
  let check_assign lvaluet rvaluet err =
    match (lvaluet, rvaluet) with
    | Float, Int -> Float
    | _, _ when lvaluet = rvaluet -> lvaluet
    | _, _ -> raise (Failure err)
  in
  let rec create_n_list v n =
    match n with 0 -> [] | 1 -> [v] | nt -> v :: create_n_list v (nt - 1) in

  let rec match_default t =
    match t with
    | Int -> SLiteral 0
    | Bool -> SBoolLit false
    | Float -> SFloatLit 0.0
    | String -> SStringLit ""
    | List (tl, len) ->
        let se = match_default tl in
        let v = create_n_list (tl, se) len in
        SList v
    | Void -> SDefaultValue
    | Func(_, _) -> SEmptyFunction
  in

  let rec check_expr e symbols local_funcs =
    match e with
    | DefaultValue -> (Void, SDefaultValue)
    | Literal l -> (Int, SLiteral l)
    | BoolLit l -> (Bool, SBoolLit l)
    | FloatLit l -> (Float, SFloatLit l)
    | StringLit l -> (String, SStringLit l)
    | Id var ->
        let t' = type_of_identifier var symbols in
        (t', SId var)
    | Assign (var, e) as ex ->
        let lt = type_of_identifier var symbols
        and rt, e' = check_expr e symbols local_funcs in
        let err =
          "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt
          ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SAssign (var, (rt, e')))
    | OpAssign (var, e, op) as ex ->
        let lt = type_of_identifier var symbols
        and rt, e' = check_expr e symbols local_funcs in
        let err =
          "illegal assignment " ^ string_of_typ lt ^ " = " ^ string_of_typ rt
          ^ " in " ^ string_of_expr ex
        in
        (check_assign lt rt err, SOpAssign (var, (lt, e'), op))
    | Binop (e1, op, e2) as e ->
        let t1, e1' = check_expr e1 symbols local_funcs
        and t2, e2' = check_expr e2 symbols local_funcs in
        let err =
          "illegal binary operator " ^ string_of_typ t1 ^ " "
          ^ string_of_binop op ^ " " ^ string_of_typ t2 ^ " in "
          ^ string_of_expr e
        in
        (* All binary operators require operands of the same type*)
        if t1 = t2 then
          (* Determine expression type based on operator and operand types *)
          let t =
            match op with
            | (Add | Sub | Multiply | Divide) when t1 = Int || t1 = Float ->
                t1
            | Add when t1 = String -> String
            | Equal | Neq -> Bool
            | (Less | LessEqual | Greater | GreaterEqual)
              when t1 = Int || t1 = Float ->
                Bool
            (* | Greater when t1 = Int || t1 = Float -> Bool *)
            | (And | Or) when t1 = Bool -> Bool
            | _ -> raise (Failure err)
          in
          (t, SBinop ((t1, e1'), op, (t2, e2')))
          (* Allows for implicit string casting, e.g. 3+"1"="31" *)
        else if op = Add && (t1 = String || t2 = String) then
          match (t1, t2) with
          | String, t | t, String -> (
            match t with
            | Int | Float ->
                (String, SBinop ((String, e1'), Add, (String, e2')))
            | _ -> raise (Failure err) )
          | _ -> raise (Failure err)
        else raise (Failure err)
    | Cast (t, e) -> (
        let t', e' = check_expr e symbols local_funcs in
        let err =
          "illegal casting from type: " ^ string_of_typ t' ^ " to type: "
          ^ string_of_typ t
        in
        match t with
        | List _ -> raise (Failure "cannot cast to a list type")
        | _ -> (check_assign t t' err, e') )
    | Unop (op, e) -> (
      match op with
      | Not ->
          let t, e' = check_expr e symbols local_funcs in
          let err =
            "illegal expression of type " ^ string_of_typ t
            ^ ", exepected boolean"
          in
          if t = Bool then (Bool, SUnop (Not, (Bool, e')))
          else raise (Failure err)
      | Neg ->
          let t, e' = check_expr e symbols local_funcs in
          let err =
            "illegal expression of type " ^ string_of_typ t
            ^ ", exepected boolean"
          in
          if t = Int || t = Float then (t, SUnop (Neg, (t, e')))
          else raise (Failure err) )
    | Call (fname, args) as call ->
        let fd = find_func fname local_funcs in
        let param_length = List.length fd.formals in
        if List.length args != param_length then
          raise
            (Failure
               ( "expecting "
               ^ string_of_int param_length
               ^ " arguments in " ^ string_of_expr call ) )
        else
          let check_call (ft, _) e =
            let et, e' = check_expr e symbols local_funcs in
            let err =
              "illegal argument found " ^ string_of_typ et ^ " expected "
              ^ string_of_typ ft ^ " in " ^ string_of_expr e
            in
            (check_assign ft et err, e')
          in
          let args' = List.map2 check_call fd.formals args in
          (fd.rtyp, SCall (fname, args'))
    | List el ->
        let len = List.length el in
        if len = 0 then (Void, SList [])
        else
          let fst_e = List.hd el in
          let fst_t', _' = check_expr fst_e symbols local_funcs in
          let type_match e =
            let t', se = check_expr e symbols local_funcs in
            if fst_t' = t' then (t', se)
            else
              raise
                (Failure
                   ( "type of elements in the list mismatch, "
                   ^ string_of_typ fst_t' ^ " != " ^ string_of_typ t' ) )
          in
          (List (fst_t', len), SList (List.map type_match el))
    | ListAccess (ln, ie) -> (
        let t, se = check_expr ie symbols local_funcs in
        if t != Int then raise (Failure "list index must be an integer")
        else
          match se with
          | SLiteral idx -> (
              let list_decl = Hashtbl.find symbols ln in
              match list_decl with
              | List (_, len) ->
                  if idx >= len || idx < 0 then
                    raise (Failure "index out of range")
                  else (t, SListAccess (ln, (Int, se)))
              | _ -> raise (Failure (ln ^ " is not a list type")) )
          | _ -> raise (Failure "") )
    | Lambda(lb_def) -> let sfunc = check_func {fname="lambda"; rtyp=lb_def.rtyp; formals=lb_def.formals; body=lb_def.body} in
        let arg_typs = List.map (fun (tp, _) -> tp) lb_def.formals in
        Func(arg_typs, lb_def.rtyp), SLambda({rtyp=sfunc.srtyp; formals=sfunc.sformals; body=sfunc.sbody})
    | LambdaApply(lb_def, el) -> let sfunc = check_func {fname="lambda"; rtyp=lb_def.rtyp; formals=lb_def.formals; body=lb_def.body} in
        let arg_typs = List.map (fun (tp, _) -> tp) lb_def.formals in
        let sel = List.map (fun e -> (check_expr e symbols local_funcs)) el in
        Func(arg_typs, lb_def.rtyp), SLambdaApply({rtyp=sfunc.srtyp; formals=sfunc.sformals; body=sfunc.sbody}, sel)
    | EmptyFunction -> Func([], Void), SEmptyFunction

  and check_bind b symbols local_funcs =
    let t, v, e = b in
    let t', e' = check_expr e symbols local_funcs in
    if e = DefaultValue then (
      let default_v = match_default t in
      match t with
      | Int -> add_symbols v t symbols; (t, v, (Int, default_v))
      | Bool -> add_symbols v t symbols; (t, v, (Bool, default_v))
      | Float -> add_symbols v t symbols; (t, v, (Float, default_v))
      | String -> add_symbols v t symbols; (t, v, (String, default_v))
      | List (tl, len) -> add_symbols v t symbols; (t, v, (List (tl, len), default_v))
      | Void -> raise (Failure "type infer can't be done")
      | Func(arg_typs, ret_typ) -> Hashtbl.add local_funcs v {rtyp=Void; formals=[]; body=[]; fname=v};
          (t, v, (Func(arg_typs, ret_typ), default_v)))
    else if t = Void then
      let _ = add_symbols v t' symbols in
      (t', v, (t', e'))
    else
      let err =
        "type mismatch. " ^ v ^ ": " ^ string_of_typ t ^ ", received "
        ^ string_of_typ t'
      in
      let tt = check_assign t t' err in
      let _ = add_symbols v tt symbols in
      (tt, v, (tt, e'))
  
  
  and check_func func =
    let _ = func.fname in
    let local_funcs = Hashtbl.create 1 in
      ignore(List.iter (fun (fm, n) -> match fm with
      | Func(arg_typs, ret_typ) -> let f_typs = List.map (fun t -> (t, "tmp")) arg_typs in
        Hashtbl.add local_funcs n {fname=n; rtyp=ret_typ; formals=f_typs; body=[]}
      | _ -> () ) func.formals);
    (* Make sure no formals or locals are void or duplicates *)
    check_dup_binds "formal" func.formals ;
    (* Build local symbol table of variables for this function *)
    let symbols = Hashtbl.create 16 in
    let globals' = List.map (fun (t, id, _) -> (t, id)) globals in
      ignore(List.iter (fun (t, id) -> Hashtbl.add symbols id t)
        (globals' @ func.formals));
    let check_bool_expr e =
      let t, e' = check_expr e symbols local_funcs in
      match t with
      | Bool -> (t, e')
      | _ ->
          raise
            (Failure ("expected Boolean expression in " ^ string_of_expr e))
    in
    let rec check_stmt_list = function
      | [] -> []
      | Block sl :: sl' -> check_stmt_list (sl @ sl') (* Flatten blocks *)
      | s :: sl ->
          let p1 = check_stmt s in
          let p2 = check_stmt_list sl in
          p1 :: p2
    (* Return a semantically-checked statement i.e. containing sexprs *)
    and check_stmt = function
      (* A block is correct if each statement is correct and nothing follows
         any Return statement. Nested blocks are flattened. *)
      | Block sl -> SBlock (check_stmt_list sl)
      | Expr e -> SExpr (check_expr e symbols local_funcs)
      | If (e, st1, st2) ->
          SIf (check_bool_expr e, check_stmt_list st1, check_stmt_list st2)
      | Ifd (e, st1) -> SIfd (check_bool_expr e, check_stmt_list st1)
      | While (e, st) -> SWhile (check_bool_expr e, check_stmt_list st)
      | Break -> SBreak
      | Continue -> SContinue
      | Return e ->
          let t, e' = check_expr e symbols local_funcs in
          if t = func.rtyp then SReturn (t, e')
          else
            raise
              (Failure
                 ( "return gives " ^ string_of_typ t ^ " expected "
                 ^ string_of_typ func.rtyp ^ " in " ^ string_of_expr e ) )
      (* var x: int *)
      (* var x: int = 2 *)
      (* var x = 2 *)
      | Bind b ->
          let sb = check_bind b symbols local_funcs in
          SBind sb
      | Empty -> SEmpty
      | For (decl, test, tail, st) ->
          let sdecl = check_bind decl symbols local_funcs in
          let stest = check_bool_expr test in
          let stail = check_expr tail symbols local_funcs in
          let sst = check_stmt_list st in
          SFor (sdecl, stest, stail, sst)
      | Switch (switch_cond, case_list) ->
          let sswitch_cond = check_expr switch_cond symbols local_funcs in
          let cond_typ, _ = sswitch_cond in
          if cond_typ != Int && cond_typ != Bool && cond_typ != String then
            raise (Failure "not supported switch condition type")
          else
            let parse_case (e, sl) =
              let se = check_expr e symbols local_funcs in
              let ssl = check_stmt_list sl in
              let case_typ, _ = se in
              if case_typ != cond_typ then
                raise (Failure "case type mismatches switch type")
              else (se, ssl)
            in
            let scase = List.map parse_case case_list in
            SSwitch (sswitch_cond, scase)
    in
    (* body of check_func *)
    { srtyp= func.rtyp
    ; sfname= func.fname
    ; sformals= func.formals
    ; sbody= check_stmt_list func.body }
  in

  let sglobals =
    (* check global variables declaration *)
    let check_globals globals =
      (* Make sure no globals duplicate *)
      let global_t = List.map (fun (t, id, _) -> (t, id)) globals in
      check_dup_binds "global" global_t;
      let global_symbols = Hashtbl.create 16 in
      List.map (fun g -> check_bind g global_symbols (Hashtbl.create 0)) globals
    in
    check_globals globals in
  
  (sglobals, List.map check_func functions)
