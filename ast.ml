type binop = Add | Sub | Equal | Neq | Less | Greater | And | Or 

type unop = Not

type typ = Int | Bool | Float | String | Void

type default_value = IntDefault | BoolDefault | FloatDefalt

type expr =
  | DefaultValue
  | Literal of int
  | BoolLit of bool
  | FloatLit of float
  | StringLit of string
  | Id of string
  | Unop of unop * expr
  | Binop of expr * binop * expr
  (* var x = 3 *)
  | Assign of string * expr
  (* var x: int = 3 *)
  (* | TypedAssign of typ * string * expr *)
  (* function call *)
  | Call of string * expr list


(* int x: name binding *)
type bind = typ * string * expr
type formal = typ * string


type stmt =
    Block of stmt list
  | Expr of expr
  | If of expr * stmt * stmt
  | While of expr * stmt
  (* return *)
  | Return of expr
  | Bind of bind
  | For of expr * expr * expr * stmt


(* func_def: ret_typ fname formals locals body *)
type func_def = {
  rtyp: typ;
  fname: string;
  formals: formal list;
  body: stmt list;
}

type program = bind list * func_def list

(* Pretty-printing functions *)
let string_of_binop = function
    Add -> "+"
  | Sub -> "-"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"


let string_of_unop = function
  Not -> "!"

let string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Void -> "void"

let rec string_of_expr = function
  | DefaultValue -> "default"
  | Literal(l) -> string_of_int l
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | FloatLit(f) -> string_of_float f
  | StringLit(s) -> s
  | Id(s) -> s
  | Unop(o, e) -> string_of_unop o ^ " " ^ string_of_expr e
  | Binop(e1, o, e2) ->
    string_of_expr e1 ^ " " ^ string_of_binop o ^ " " ^ string_of_expr e2
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  (* | TypedAssign(t, v, e) -> v ^ ":" ^ string_of_typ t ^ " = " ^ string_of_expr e *)
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"


let string_of_bind(b) = let (t, id, e) = b in "var " ^ id ^ ": " ^ string_of_typ t ^ string_of_expr e ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ")\n" ^
                      string_of_stmt s1 ^ "else\n" ^ string_of_stmt s2
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") " ^ string_of_stmt s
  | Bind(b) -> string_of_bind b
  | For(e1, e2, e3, st) -> "not implemented"


let string_of_fdecl fdecl =
  string_of_typ fdecl.rtyp ^ " " ^
  fdecl.fname ^ "(" ^ String.concat ", " (List.map snd fdecl.formals) ^
  ")\n{\n" ^
  String.concat "" (List.map string_of_stmt fdecl.body) ^
  "}\n"

let string_of_program (vars, funcs) =
  "\n\nParsed program: \n\n" ^
  String.concat "" (List.map string_of_bind vars) ^ "\n" ^
  String.concat "\n" (List.map string_of_fdecl funcs)
