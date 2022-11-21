%{
	open Ast
%}

%token SEMI NEWLINE LPAREN RPAREN LBRACKET RBRACKET LBRACE RBRACE 
%token PLUS MINUS MULTIPLY DIVIDE MOD ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ
%token EQ NEQ LT GT AND OR NOT 
%token IF ELSE WHILE INT BOOL
/* return, COMMA token */
%token RETURN COMMA
%token VAR FOR COLON STRING FLOAT VOID LIST MAP FUNC CONT BREAK
%token IFX
%token <int> LITERAL
%token <bool> BLIT
%token <float> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN

%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS
%left MULTIPLY DIVIDE MOD 
%nonassoc IFX
%nonassoc NOT
%nonassoc LPAREN RPAREN
%nonassoc ELSE
%nonassoc ID LITERAL BLIT FLIT SLIT
%nonassoc NEWLINE



%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | NEWLINE decls	{ (fst $2, snd $2) }
 | vdecl NEWLINE decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

// var x: string;
// var x = 1
// foo(x: int)
vdecl:
 | VAR ID COLON typ { ($4, $2, DefaultValue) }
 | VAR ID COLON typ ASSIGN expr { ($4, $2, $6) }
 | VAR ID ASSIGN expr { (Void, $2, $4) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | STRING { String }
  | VOID  { Void }
  | typ LBRACKET RBRACKET { List($1) }

/* fdecl */
fdecl:
  FUNC ID LPAREN formals_opt RPAREN COLON typ LBRACE stmt_list RBRACE
  {
    {
      rtyp=$7;
      fname=$2;
      formals=$4;
      body=$9;
    }
  }
  | FUNC ID LPAREN formals_opt RPAREN LBRACE stmt_list RBRACE
  {
	{
      rtyp=Void;
      fname=$2;
      formals=$4;
      body=$7;
    }
  }


/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }


formals_list:
  formal_vdecl { [$1] }
  | formal_vdecl COMMA formals_list { $1::$3 }

formal_vdecl:
  ID COLON typ	{ ($3, $1) }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }


stmt:
  | expr NEWLINE                           	{ Expr $1  }
  | NEWLINE									{ Empty }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if condition { block1} else {block2} */
  /* if condition { stmt } else { stmt } */
  | IF expr LBRACE stmt_list RBRACE			{ Ifd($2, $4) } 
  | IF expr LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE   { If($2, $4, $8) }
  
  /* while (condition) stmt */
  | WHILE expr LBRACE stmt_list RBRACE          { While($2, $4)  }
  /* return */
  | RETURN expr NEWLINE                        { Return $2      }
  | FOR vdecl SEMI expr SEMI expr LBRACE stmt_list RBRACE	{ For($2, $4, $6, $8) }
  | vdecl NEWLINE									{ Bind($1) }

  
expr:
    LITERAL          { Literal($1)            }
  | BLIT             { BoolLit($1)            }
  | FLIT			{ FloatLit($1) }
  | SLIT			{ StringLit($1) }
  | ID               { Id($1)                 }
  | expr PLUS   expr { Binop($1, Add,   $3)   }
  | expr MINUS  expr { Binop($1, Sub,   $3)   }
  | expr EQ     expr { Binop($1, Equal, $3)   }
  | expr NEQ    expr { Binop($1, Neq, $3)     }
  | expr MULTIPLY expr	{ Binop($1, Multiply, $3) }
  | expr DIVIDE expr { Binop($1, Divide, $3) }
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | NOT expr		{ Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3)    	}
  | LPAREN expr RPAREN { $2                   }
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | LBRACKET args_opt RBRACKET	{ List($2) }
//  var x = foo(a)
  /* call */

/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
