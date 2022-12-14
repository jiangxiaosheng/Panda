module L = Llvm
module A = Ast
open Sast
module StringMap = Map.Make (String)

let translate (_, functions) =
  let context = L.global_context () in
  let the_module = L.create_module context "Panda" in
  (* Get types from the context *)
  let i32_t = L.i32_type context
  and i8_t = L.i8_type context
  and i1_t = L.i1_type context
  and float_t = L.double_type context
  and void_t = L.void_type context in
  let string_t = L.pointer_type i8_t in
  let rec ltype_of_typ = function
    | A.Int -> i32_t
    | A.Bool -> i1_t
    | A.Float -> float_t
    | A.Void -> void_t
    | A.String -> string_t
    | A.List (t, _) -> L.pointer_type (ltype_of_typ t)
    | _ -> raise (Failure "not implemented yet")
  in
  (* global variables *)
  (* let global_vars = let global_var m (t, n, _) = let init = match t with
     A.Float -> L.const_float (ltype_of_typ t) 0.0 | _ -> L.const_int
     (ltype_of_typ t) 0 in StringMap.add n (L.define_global n init
     the_module) m in List.fold_left global_var StringMap.empty globals in *)

  (* let global_vars = Hashtbl.create 4 in *)
  (* let add_global_var (tp, n, e) = L.define_global n *)
  let builtin_funcs = Hashtbl.create 4 in
  let printf_t : L.lltype =
    L.var_arg_function_type i32_t [|L.pointer_type i8_t|]
  in
  let printf_func : L.llvalue =
    L.declare_function "printf" printf_t the_module
  in
  ignore (Hashtbl.add builtin_funcs "printf" printf_func) ;
  let println_t : L.lltype =
    L.var_arg_function_type void_t [|L.pointer_type i8_t|]
  in
  let println_func : L.llvalue =
    L.declare_function "println" println_t the_module
  in
  ignore (Hashtbl.add builtin_funcs "println" println_func) ;
  let assert_t : L.lltype =
    L.function_type void_t [|i1_t; L.pointer_type i8_t|]
  in
  let assert_func : L.llvalue =
    L.declare_function "assert" assert_t the_module
  in
  ignore (Hashtbl.add builtin_funcs "assert" assert_func) ;
  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      (* array of lltype, e.g. [i32_t, float_t] *)
      and formal_types =
        Array.of_list
          (List.map (fun (t, _) -> ltype_of_typ t) fdecl.sformals)
        (* create the function type, 1st arg is the return type represented
           by a lltype, 2nd arg is the formals represented by an array of
           lltype *)
      in
      let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m
    in
    List.fold_left function_decl StringMap.empty functions
  in
  let build_function_body fdecl =
    (* TODO: build function_decls *)
    let the_function, _ = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in
    (* let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder and
       float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in *)

    (* local_vars initially only includes the formals local variables will be
       added later when we deal with statements *)
    let local_vars = Hashtbl.create 16 in
    let add_formal (t, n) p =
      L.set_value_name n p ;
      let local = L.build_alloca (ltype_of_typ t) n builder in
      ignore (L.build_store p local builder) ;
      ignore (Hashtbl.add local_vars n local)
    in
    ignore
      (List.iter2 add_formal fdecl.sformals
         (Array.to_list (L.params the_function)) ) ;
    let lookup n =
      try Hashtbl.find local_vars n with Not_found -> raise (Failure "todo")
      (* Hashtbl.find global_vars n *)
    in
    let rec build_expr builder ((_, e) : sexpr) =
      match e with
      | SLiteral i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      | SStringLit s -> L.const_string context s
      | SId id -> L.build_load (lookup id) id builder
      | SAssign (s, e) ->
          let e' = build_expr builder e in
          ignore (L.build_store e' (lookup s) builder) ;
          e'
      | SBinop (((A.Float, _) as e1), op, e2) ->
          let e1' = build_expr builder e1 and e2' = build_expr builder e2 in
          ( match op with
          | A.Add -> L.build_fadd
          | A.Sub -> L.build_fsub
          | A.Multiply -> L.build_fmul
          | A.Divide -> L.build_fdiv
          | A.Equal -> L.build_fcmp L.Fcmp.Oeq
          | A.Neq -> L.build_fcmp L.Fcmp.One
          | A.Less -> L.build_fcmp L.Fcmp.Olt
          | A.LessEqual -> L.build_fcmp L.Fcmp.Ole
          | A.Greater -> L.build_fcmp L.Fcmp.Ogt
          | A.GreaterEqual -> L.build_fcmp L.Fcmp.Oge
          | A.And | A.Or ->
              raise
                (Failure
                   "internal error: semant should have rejected and/or on \
                    float" ) )
            e1' e2' "tmp" builder
      | SBinop (e1, op, e2) ->
          let e1' = build_expr builder e1 and e2' = build_expr builder e2 in
          ( match op with
          | A.Add -> L.build_add
          | A.Sub -> L.build_sub
          | A.Multiply -> L.build_mul
          | A.Divide -> L.build_sdiv
          | A.Equal -> L.build_icmp L.Icmp.Eq
          | A.Neq -> L.build_icmp L.Icmp.Ne
          | A.Less -> L.build_icmp L.Icmp.Slt
          | A.LessEqual -> L.build_icmp L.Icmp.Sle
          | A.Greater -> L.build_icmp L.Icmp.Sgt
          | A.GreaterEqual -> L.build_icmp L.Icmp.Sge
          | A.And -> L.build_and
          | A.Or -> L.build_or )
            e1' e2' "tmp" builder
      | SUnop (op, ((t, _) as e)) ->
          let e' = build_expr builder e in
          ( match op with
          | A.Neg when t = A.Float -> L.build_fneg
          | A.Neg -> L.build_neg
          | A.Not -> L.build_not )
            e' "tmp" builder
      (* fixme *)
      | SCast se ->
          let tp, _ = se in
          let e' = build_expr builder se in
          let tmp = L.build_alloca (ltype_of_typ tp) "tmp" builder in
          ignore (L.build_store e' tmp builder) ;
          e'
      | SOpAssign (id, ((A.Float, _) as e), op) ->
          let e' = build_expr builder e in
          let id_v = L.build_load (lookup id) id builder in
          let res =
            ( match op with
            | A.AddEq -> L.build_fadd
            | A.SubEq -> L.build_fsub
            | A.MultEq -> L.build_fmul
            | A.DivEq -> L.build_fdiv
            | _ ->
                raise
                  (Failure
                     "syntax error: not allowed operator on int operands" )
            )
              id_v e' "tmp" builder
          in
          ignore (L.build_store res (lookup id) builder) ;
          res
      | SOpAssign (id, e, op) ->
          let e' = build_expr builder e in
          let id_v = L.build_load (lookup id) id builder in
          let res =
            ( match op with
            | A.AddEq -> L.build_add
            | A.SubEq -> L.build_sub
            | A.MultEq -> L.build_mul
            | A.DivEq -> L.build_sdiv
            | _ ->
                raise
                  (Failure
                     "syntax error: not allowed operator on int operands" )
            )
              id_v e' "tmp" builder
          in
          ignore (L.build_store res (lookup id) builder) ;
          res
      | SCall (f, args) ->
          let fdef, fdecl = StringMap.find f function_decls in
          let llargs =
            List.rev (List.map (build_expr builder) (List.rev args))
          in
          let result =
            match fdecl.srtyp with A.Void -> "" | _ -> f ^ "_result"
          in
          L.build_call fdef (Array.of_list llargs) result builder
          (* TODO *)
      | SList l ->
          let len = List.length l in
          if len = 0 then
            L.build_array_malloc void_t (L.const_int i32_t 0) "tmp" builder
          else
            let s_tp, _ = List.hd l in
            L.build_array_malloc (ltype_of_typ s_tp) (L.const_int i32_t 0)
              "tmp" builder
      (* | SListAccess(l, idx) -> let _, e = idx in let se = build_expr builder idx in begin
          match e with
          | SLiteral i -> L.build_extractvalue (lookup l) i "tmp" builder
          | _ -> raise (Failure "index is not an int type")
          end *)
      | _ -> raise (Failure "expr not implemented")
    in
    let add_terminal builder instr =
      match L.block_terminator (L.insertion_block builder) with
      | Some _ -> ()
      | None -> ignore (instr builder)
    in
    let rec build_stmt builder = function
      | SBlock sl -> List.fold_left build_stmt builder sl
      | SEmpty -> builder
      | SExpr e ->
          ignore (build_expr builder e) ;
          builder
      | SBind sb ->
          let tp, id, se = sb in
          let se' = build_expr builder se in
          let local = L.build_alloca (ltype_of_typ tp) id builder in
          ignore (Hashtbl.add local_vars id local) ;
          ignore (L.build_store se' (lookup id) builder) ;
          builder
      | SReturn e ->
          ignore
            ( match fdecl.srtyp with
            (* Special "return nothing" instr *)
            | A.Void -> L.build_ret_void builder (* Build return statement *)
            | _ -> L.build_ret (build_expr builder e) builder ) ;
          builder
      | SIf (predicate, then_stmt_list, else_stmt_list) ->
          let bool_val = build_expr builder predicate in
          let then_bb = L.append_block context "then" the_function in
          let else_bb = L.append_block context "else" the_function in
          let end_bb = L.append_block context "if_end" the_function in
          ignore (L.build_cond_br bool_val then_bb else_bb builder) ;
          ignore
            (List.iter
               (fun stmt ->
                 ignore (build_stmt (L.builder_at_end context then_bb) stmt)
                 )
               then_stmt_list ) ;
          ignore
            (List.iter
               (fun stmt ->
                 ignore (build_stmt (L.builder_at_end context else_bb) stmt)
                 )
               else_stmt_list ) ;
          let build_br_end = L.build_br end_bb in
          add_terminal (L.builder_at_end context then_bb) build_br_end ;
          add_terminal (L.builder_at_end context else_bb) build_br_end ;
          L.builder_at_end context end_bb
      | SIfd (predicate, then_stmt_list) ->
          let bool_val = build_expr builder predicate in
          let then_bb = L.append_block context "then" the_function in
          let end_bb = L.append_block context "if_end" the_function in
          ignore (L.build_cond_br bool_val then_bb end_bb builder) ;
          ignore
            (List.iter
               (fun stmt ->
                 ignore (build_stmt (L.builder_at_end context then_bb) stmt)
                 )
               then_stmt_list ) ;
          let build_br_end = L.build_br end_bb in
          add_terminal (L.builder_at_end context then_bb) build_br_end ;
          L.builder_at_end context end_bb
          (* WHILE: predicate.code || con_br(condition jump) predicate.addr
             BODY END

             BODY: ... body.code || jmp WHILE

             END: .... *)
      | SWhile (predicate, body) ->
          let while_bb = L.append_block context "while" the_function in
          let body_bb = L.append_block context "while_body" the_function in
          let end_bb = L.append_block context "while_end" the_function in
          let build_br_while = L.build_br while_bb in
          ignore (build_br_while builder) ;
          let bool_val =
            build_expr (L.builder_at_end context while_bb) predicate
          in
          ignore
            (L.build_cond_br bool_val body_bb end_bb
               (L.builder_at_end context while_bb) ) ;
          ignore
            (List.iter
               (fun stmt ->
                 match stmt with
                 | SBreak ->
                     ignore
                       (L.build_br end_bb (L.builder_at_end context body_bb))
                 | SContinue ->
                     ignore
                       (L.build_br while_bb
                          (L.builder_at_end context body_bb) )
                 | _ ->
                     ignore
                       (build_stmt (L.builder_at_end context body_bb) stmt)
                 )
               body ) ;
          add_terminal (L.builder_at_end context body_bb) build_br_while ;
          L.builder_at_end context end_bb
      (* Implement for loops as while loops *)
      | SFor (e1, e2, e3, body) ->
          build_stmt builder
            (SBlock [SBind e1; SWhile (e2, List.append body [SExpr e3])])
      | SBreak -> raise (Failure "syntax error: break not appear in a loop")
      | SContinue ->
          raise (Failure "syntax error: continue not appear in a loop")
      (* | SSwitch (sswitch_cond, scase_list) -> let llswitch = L.build_switch (build_expr builder sswitch_cond)
          (L.append_block context "switch_else" the_function)
          (List.length scase_list) builder in let end_bb = L.append_block context "switch_end" the_function in
            ignore(L.position_at_end end_bb builder);
          List.iter2 (fun case ->
            let dest_bb = L.append_block context "switch_case" the_function in
              ignore(L.position_at_end dest_bb builder);
            let llswitch (L.const_int )) *)
      | _ -> raise (Failure "not implemented yet")
    in
    (* Build the code for each statement in the function *)
    let builder = build_stmt builder (SBlock fdecl.sbody) in
    (* Add a return if the last block falls off the end *)
    add_terminal builder
      ( match fdecl.srtyp with
      | A.Void -> L.build_ret_void
      | A.Float -> L.build_ret (L.const_float float_t 0.0)
      | t -> L.build_ret (L.const_int (ltype_of_typ t) 0) )
  in
  List.iter build_function_body functions ;
  the_module
