module L = Llvm
module A = Ast
open Sast

module StringMap = Map.Make(String)

let translate (globals, functions) =
  let context    = L.global_context () in
  let the_module = L.create_module context "Panda" in

  (* Get types from the context *)
  let i32_t      = L.i32_type    context
  and i8_t       = L.i8_type     context
  and i1_t       = L.i1_type     context
  and float_t    = L.double_type context
  and void_t     = L.void_type   context in

  let string_t   = L.pointer_type i8_t in

  let ltype_of_typ = function
      A.Int   -> i32_t
    | A.Bool  -> i1_t
    | A.Float -> float_t
    | A.Void  -> void_t
    (* | A.String ->  *)
  in

  let global_vars : L.llvalue StringMap.t =
    let global_var m (t, n, _) = 
      let init = match t with
          A.Float -> L.const_float (ltype_of_typ t) 0.0
        | _ -> L.const_int (ltype_of_typ t) 0
      in StringMap.add n (L.define_global n init the_module) m in
    List.fold_left global_var StringMap.empty globals in


  let function_decls : (L.llvalue * sfunc_def) StringMap.t =
    let function_decl m fdecl =
      let name = fdecl.sfname
      (* array of lltype, e.g. [i32_t, float_t] *)
      and formal_types = 
        Array.of_list (List.map (fun (t,_) -> ltype_of_typ t) fdecl.sformals)
      (* create the function type, 1st arg is the return type represented by a lltype,
         2nd arg is the formals represented by an array of lltype *)
      in let ftype = L.function_type (ltype_of_typ fdecl.srtyp) formal_types in
      StringMap.add name (L.define_function name ftype the_module, fdecl) m in
    List.fold_left function_decl StringMap.empty functions in
  

  let build_function_body fdecl =
    (* TODO: build function_decls *)
    let (the_function, _) = StringMap.find fdecl.sfname function_decls in
    let builder = L.builder_at_end context (L.entry_block the_function) in

    (* let int_format_str = L.build_global_stringptr "%d\n" "fmt" builder
    and float_format_str = L.build_global_stringptr "%g\n" "fmt" builder in *)

    (* local_vars initially only includes the formals
       local variables will be added later when we deal with statements *)
    let local_vars =
      let add_formal m (t, n) p = 
        L.set_value_name n p;
	      let local = L.build_alloca (ltype_of_typ t) n builder in
          ignore (L.build_store p local builder);
	      StringMap.add n local m 
    
      in List.fold_left2 add_formal StringMap.empty fdecl.sformals
            (Array.to_list (L.params the_function)) in
    
    let lookup n = try StringMap.find n local_vars
      with Not_found -> StringMap.find n global_vars
    in

    let rec build_expr builder ((_, e) : sexpr) = match e with
      SLiteral i -> L.const_int i32_t i
      | SBoolLit b -> L.const_int i1_t (if b then 1 else 0)
      | SFloatLit f -> L.const_float float_t f
      (* | SStringLit s -> L.const_string string_t s *)
      | SId id -> L.build_load (lookup id) id builder
      | SAssign(s, e) -> let e' = build_expr builder e in
              ignore(L.build_store e' (lookup s) builder); e'
      | 
      
