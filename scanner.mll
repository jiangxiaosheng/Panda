{
	open Parser
	open Buffer

	let string_buff = Buffer.create 32

	let escape_to_char = function
	 | 'n' -> '\010'
	 | 'r' -> '\013'
	 | 'b' -> '\008'
	 | 't' -> '\009'
	 | c   -> c
}

let digit = ['0'-'9']
let sign = ['-''+']
let integer = sign?digit+
let decimal = sign?(digit+'.'digit*)|('.'digit+)
let letter = ['a'-'z' 'A'-'Z']
(* reference: https://stackoverflow.com/questions/66307896/lexing-strings-in-ocamllex *)
let backslash_escapes = ['\\' '\'' '"' 'n' 't' 'b' 'r']



rule token = parse
  [' ' '\t' '\r'] { token lexbuf } (* Whitespace *)
| "/*"     { multi_comment lexbuf }           (* Comments *)
| "//"		{ single_comment lexbuf }
| '\n'+		{ NEWLINE }
| "var"		{ VAR }
| "for"		{ FOR }
| '('      { LPAREN }
| ')'      { RPAREN }
| '['		{ LBRACKET }
| ']'		{ RBRACKET }
| '{'      { LBRACE }
| '}'      { RBRACE }
| ';'      { SEMI }
| ','      { COMMA }
| '+'      { PLUS }
| '-'      { MINUS }
| '*'		{ MULTIPLY }
| '/'		{ DIVIDE }
| '%'		{ MOD }
| '='      { ASSIGN }
| ":"		{ COLON }
| "=="     { EQ }
| "!="     { NEQ }
| '!'		{ NOT }
| "+="		{ PLUSASSIGN }
| "-="		{ MINUASSIGN }
| "*="		{ MULTASSIGN }
| "/="		{ DIVASSIGN }
| '<'      { LT }
| '>'	   { GT }
| "&&"     { AND }
| "||"     { OR }
| "if"     { IF }
| "else"   { ELSE }
| "while"  { WHILE }
| "return" { RETURN }
| "int"    { INT }
| "boolean"   { BOOL }
| "string"	{ STRING }
| "float"	{ FLOAT }
| "void"	{ VOID }
| "list"	{ LIST }
| "map"		{ MAP }
| "func"	{ FUNC }
| "continue"	{ CONT }
| "break"	{ BREAK }
| "true"   { BLIT(true)  }
| "false"  { BLIT(false) }
| '"'		{ Buffer.clear string_buff;
			  string lexbuf;
			  SLIT(Buffer.contents string_buff) }
| integer as i { LITERAL(int_of_string i) }
| decimal as f		{ FLIT(float_of_string f) }
| letter (digit | letter | '_')* as lem { ID(lem) }
| eof { EOF }
| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }

and multi_comment = parse
  "*/" { token lexbuf }
| _    { multi_comment lexbuf }

and single_comment = parse
  '\n'	{ token lexbuf }
| _		{ single_comment lexbuf }

and string = parse
| '"'	{ () }
| '\\' (backslash_escapes as e)	{ Buffer.add_char string_buff (escape_to_char e); string lexbuf }
| _ as c	{ Buffer.add_char string_buff c; string lexbuf }
