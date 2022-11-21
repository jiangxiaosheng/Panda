type binop = Add | Sub | Multiply | Divide | Equal | Neq | Less | Greater | And | Or 

type unop = Not

type assignop = AddEq | SubEq | MultEq | DivEq

type typ = Int | Bool | Float | String | Void | List of typ

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
  | OpAssign of string * expr * assignop
  (* var x: int = 3 *)
  | List of expr list
  (* function call *)
  | Call of string * expr list


(* int x: name binding *)
type bind = typ * string * expr
type formal = typ * string


type stmt =
    Block of stmt list
  | Empty
  | Expr of expr
  | If of expr * stmt list * stmt list
  | Ifd of expr * stmt list
  | While of expr * stmt list
  (* return *)
  | Break | Continue
  | Return of expr
  | Bind of bind
  | For of bind * expr * expr * stmt list


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
  | Multiply -> "*"
  | Divide -> "/"
  | Equal -> "=="
  | Neq -> "!="
  | Less -> "<"
  | Greater -> ">"
  | And -> "&&"
  | Or -> "||"


let string_of_unop = function
  Not -> "!"

let string_of_assignop = function
  AddEq -> "+="
  | SubEq -> "-="
  | MultEq -> "*="
  | DivEq -> "/="

let rec string_of_typ = function
    Int -> "int"
  | Bool -> "bool"
  | Float -> "float"
  | String -> "string"
  | Void -> "void"
  | List t -> "list of " ^ string_of_typ t

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
  | List(el) -> "[" ^ String.concat ", " (List.map string_of_expr el) ^ "]"
  | OpAssign(v, e, op) -> v ^ " " ^ string_of_assignop op ^ " " ^ string_of_expr e


let string_of_bind(b) = let (t, id, e) = b in "var " ^ id ^ ": " ^ string_of_typ t ^ " = " ^string_of_expr e ^ ";\n"

let rec string_of_stmt = function
    Block(stmts) ->
    "{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
  | Expr(expr) -> string_of_expr expr ^ ";\n"
  | Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
  | If(e, s1, s2) -> "if (" ^ string_of_expr e ^ ") {\n" ^
    String.concat "" (List.map string_of_stmt s1) ^ "} " ^ "else {" ^ 
    String.concat "" (List.map string_of_stmt s1) ^ "}\n"
  | Ifd(e, s1) ->  "if (" ^ string_of_expr e ^ ") {" ^
    String.concat "" (List.map string_of_stmt s1) ^ "}\n"
  | While(e, s) -> "while (" ^ string_of_expr e ^ ") {\n" ^ 
    String.concat "" (List.map string_of_stmt s) ^ "}\n"
  | Bind(b) -> string_of_bind b
  | For(e1, e2, e3, st) -> "for (" ^ string_of_bind e1 ^ " " ^ string_of_expr e2 ^ "; " ^ string_of_expr e3 ^
                          ")\n" ^ String.concat "\n" (List.map string_of_stmt st)
  | Empty -> ""
  | Break -> "break()\n" 
  | Continue -> "continue()\n"


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
