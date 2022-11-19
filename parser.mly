%{
	open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE PLUS MINUS ASSIGN PLUSEQ MINUSEQ STAREQ SLASHEQ
%token EQ NEQ LT GT AND OR NOT
%token IF ELSE WHILE INT BOOL
/* return, COMMA token */
%token RETURN COMMA
%token VAR FOR COLON STRING FLOAT VOID LIST MAP FUNC CONT BREAK
%token <int> LITERAL
%token <bool> BLIT
%token <float> FLIT
%token <string> SLIT
%token <string> ID
%token EOF

%start program
%type <Ast.program> program

%right ASSIGN
%nonassoc NOT
%left OR
%left AND
%left EQ NEQ
%left LT GT
%left PLUS MINUS


%%

/* add function declarations*/
program:
  decls EOF { $1}

decls:
   /* nothing */ { ([], [])               }
 | vdecl SEMI decls { (($1 :: fst $3), snd $3) }
 | fdecl decls { (fst $2, ($1 :: snd $2)) }

vdecl_list:
  /*nothing*/ { [] }
  | vdecl SEMI vdecl_list  {  $1 :: $3 }

// var x: string;
vdecl:
  VAR ID COLON typ { ($4, $2) }

typ:
    INT   { Int   }
  | BOOL  { Bool  }
  | FLOAT { Float }
  | STRING { String }
  | VOID  { Void }

/* fdecl */
fdecl:
  typ ID LPAREN formals_opt RPAREN LBRACE vdecl_list stmt_list RBRACE
  {
    {
      rtyp=$1;
      fname=$2;
      formals=$4;
      locals=$7;
      body=$8
    }
  }

/* formals_opt */
formals_opt:
  /*nothing*/ { [] }
  | formals_list { $1 }


formals_list:
  vdecl { [$1] }
  | vdecl COMMA formals_list { $1::$3 }

// formal_vdecl:
//   ID COLON typ	{ ($3, $1) }

stmt_list:
  /* nothing */ { [] }
  | stmt stmt_list  { $1::$2 }

stmt:
    expr SEMI                               { Expr $1  }
  | LBRACE stmt_list RBRACE                 { Block $2 }
  /* if (condition) { block1} else {block2} */
  /* if (condition) stmt else stmt */
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  /* while (condition) stmt */
  | WHILE LPAREN expr RPAREN stmt           { While ($3, $5)  }
  /* return */
  | RETURN expr SEMI                        { Return $2      }
  
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
  | expr LT     expr { Binop($1, Less,  $3)   }
  | expr GT     expr { Binop($1, Greater,  $3)   }
  | expr AND    expr { Binop($1, And,   $3)   }
  | expr OR     expr { Binop($1, Or,    $3)   }
  | ID LPAREN args_opt RPAREN { Call ($1, $3)  }
  | NOT expr		{ Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3)    	}
//   | ID COLON typ ASSIGN expr	{ TypedAssign($4, $2, $6) }
  | LPAREN expr RPAREN { $2                   }
  /* call */


/* args_opt*/
args_opt:
  /*nothing*/ { [] }
  | args { $1 }

args:
  expr  { [$1] }
  | expr COMMA args { $1::$3 }
