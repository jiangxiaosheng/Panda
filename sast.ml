open Ast

type sexpr = typ * sx
and sx =
  | SDefaultValue
  | SLiteral of int
  | SBoolLit of bool
  | SFloatLit of float
  | SStringLit of string
  | SId of string
  | SBinop of sexpr * binop * sexpr
  | SUnop of unop * sexpr
  | SAssign of string * sexpr
  (* call *)
  | SCall of string * sexpr list


type sbind = typ * string * sexpr

type sstmt =
    SBlock of sstmt list
  | SEmpty
  | SExpr of sexpr
  | SIf of sexpr * sstmt * sstmt
  | SIfd of sexpr * sstmt
  | SWhile of sexpr * sstmt
  (* return *)
  | SReturn of sexpr
  | SBind of sbind
  | SFor of sexpr * sexpr * sexpr * sstmt


type sfunc_def = {
  srtyp: typ;
  sfname: string;
  sformals: formal list;
  sbody: sstmt list;
}

type sprogram = sbind list * sfunc_def list


let rec string_of_sexpr (t, e) =
  "(" ^ string_of_typ t ^ " : " ^ (match e with
      | SDefaultValue -> "default"
      | SLiteral(l) -> string_of_int l
      | SBoolLit(true) -> "true"
      | SBoolLit(false) -> "false"
      | SFloatLit(f) -> string_of_float f
      | SStringLit(s) -> s
      | SId(s) -> s
      | SBinop(e1, o, e2) ->
        string_of_sexpr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_sexpr e2
      | SUnop(o, e) -> string_of_unop o ^ string_of_sexpr e
      | SAssign(v, e) -> v ^ " = " ^ string_of_sexpr e
      | SCall(f, el) ->
          f ^ "(" ^ String.concat ", " (List.map string_of_sexpr el) ^ ")"
  ) ^ ")"
      


let string_of_sbind(b) = let (t, id, e) = b in "var " ^ id ^ ": " ^ string_of_typ t ^ string_of_sexpr e ^ ";\n"

let rec string_of_sstmt = function
    SBlock(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_sstmt stmts) ^ "}\n"
  | SExpr(expr) -> string_of_sexpr expr ^ ";\n"
  | SReturn(expr) -> "return " ^ string_of_sexpr expr ^ ";\n"
  | SIf(e, s1, s2) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
                       string_of_sstmt s1 ^ "else\n" ^ string_of_sstmt s2
  | SIfd(e, s1) ->  "if (" ^ string_of_sexpr e ^ ")\n" ^
    string_of_sstmt s1 
  | SWhile(e, s) -> "while (" ^ string_of_sexpr e ^ ") " ^ string_of_sstmt s
  | SBind(b) -> string_of_sbind b
  | SFor(e1, e2, e3, st) -> "not implemented"
  | SEmpty -> ""

let string_of_sfdecl fdecl =
  string_of_typ fdecl.srtyp ^ " " ^
  fdecl.sfname ^ "(" ^ String.concat ", " (List.map snd fdecl.sformals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_sstmt fdecl.sbody) ^
  "}\n"

let string_of_sprogram (vars, funcs) =
  "\n\nSementically checked program: \n\n" ^
  String.concat "" (List.map string_of_sbind vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_sfdecl funcs)