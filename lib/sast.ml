open Ast

type sexpr = typ * sx

and sx =
  | SDefaultValue
  | SEmptyFunction
  | SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr
  | SCast of sexpr
  | SAssign of string * sexpr
  | SOpAssign of string * sexpr * assignop
  (* call *)
  | SCall of string * sexpr list
  | SList of sexpr list
  | SListAccess of string * sexpr
  | SLambda of slambda_def
  | SLambdaApply of slambda_def * sexpr list

and sbind = typ * string * sexpr
and slambda_def =
  {rtyp: typ; formals: formal list; body: sstmt list}

and sstmt =
  | SBlock of sstmt list
  | SEmpty
  | SExpr of sexpr
  | SIf of sexpr * sstmt list * sstmt list
  | SIfd of sexpr * sstmt list
  | SWhile of sexpr * sstmt list
  | SBreak
  | SContinue
  (* return *)
  | SReturn of sexpr
  | SBind of sbind
  (* first arg may not be a sbind, it could be an assign too *)
  | SFor of sbind * sexpr * sexpr * sstmt list
  | SSwitch of sexpr * (sexpr * sstmt list) list

type sfunc_def =
  {srtyp: typ; sfname: string; sformals: formal list; sbody: sstmt list}

type sprogram = sbind list * sfunc_def list

let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : "
  ^ ( match e with
    | SDefaultValue -> "default"
    | SLiteral l -> string_of_int l
    | SBoolLit true -> "true"
    | SBoolLit false -> "false"
    | SFloatLit f -> string_of_float f
    | SStringLit s -> s
    | SId s -> s
    | SBinop (e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " "
        ^ string_of_sexpr e2
    | SCast e -> string_of_sexpr e
    | SUnop (o, e) -> string_of_unop o ^ string_of_sexpr e
    | SAssign (v, e) -> v ^ " = " ^ string_of_sexpr e
    | SOpAssign (v, e, op) ->
        v ^ " " ^ string_of_assignop op ^ " " ^ string_of_sexpr e
    | SCall (f, el) ->
        f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
    | SList el ->
        "[" ^ String.concat ", " (List.map string_of_sexpr el) ^ "]"
    | SListAccess (n, idx) -> n ^ "[" ^ string_of_sexpr idx ^ "]"
    | SLambda(slb_def) -> "lambda: (" ^ String.concat "," (List.map (fun (t, _) -> string_of_typ t) slb_def.formals) 
        ^ ") : " ^ string_of_typ slb_def.rtyp ^ " {\n"
        ^ String.concat "" (List.map string_of_sstmt slb_def.body) ^ " }\n"
    | SLambdaApply(slb_def, se) ->  "lambda: (" ^ String.concat "," (List.map (fun (t, _) -> string_of_typ t) slb_def.formals) 
    ^ ") : " ^ string_of_typ slb_def.rtyp ^ " {\n"
    ^ String.concat "" (List.map string_of_sstmt slb_def.body) ^ " }\n"
    ^ " (" ^ String.concat ", " (List.map string_of_sexpr se) ^ ")"
    | _ -> "" )
  ^ ")"

and string_of_sbind b =
  let t, id, e = b in
  "var " ^ id ^ ": " ^ string_of_typ t ^ string_of_sexpr e ^ "\n"

and string_of_sstmt = function
  | SBlock stmts ->
      "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr expr -> string_of_sexpr expr ^ "\n"
  | SReturn expr -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf (e, s1, s2) ->
      "if (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "" (List.map string_of_sstmt s1)
      ^ "} " ^ "else {\n"
      ^ String.concat "" (List.map string_of_sstmt s2)
      ^ "}\n"
  | SIfd (e, s1) ->
      "if (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "" (List.map string_of_sstmt s1)
      ^ "}\n"
  | SWhile (e, s) ->
      "while (" ^ string_of_sexpr e ^ ") {\n"
      ^ String.concat "" (List.map string_of_sstmt s)
      ^ "}\n"
  | SBind b -> string_of_sbind b
  | SFor (sb, test, tail, st) ->
      "for (" ^ string_of_sbind sb ^ "; " ^ string_of_sexpr test ^ "; "
      ^ string_of_sexpr tail ^ ") {\n"
      ^ String.concat "" (List.map string_of_sstmt st)
      ^ "}\n"
  | SSwitch (e, el) ->
      "switch (" ^ string_of_sexpr e ^ ") {"
      ^ String.concat ""
          (List.map
             (fun (ce, stl) ->
               " case: " ^ string_of_sexpr ce ^ "{\n"
               ^ String.concat "" (List.map string_of_sstmt stl) )
             el )
      ^ "}\n"
  | SEmpty -> ""
  | SBreak -> "break()\n"
  | SContinue -> "continue()\n"

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^ fdecl.sfname ^ "("
  ^ String.concat ", " (List.map snd fdecl.sformals)
  ^ ") {\n"
  ^ String.concat "" (List.map string_of_sstmt fdecl.sbody)
  ^ "}\n"

let string_of_sprogram (vars, funcs) =
  "Sementically checked program: \n\n"
  ^ String.concat "" (List.map string_of_sbind vars)
  ^ "\n"
  ^ String.concat "\n" (List.map string_of_sfdecl funcs)
