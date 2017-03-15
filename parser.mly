/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

/* TODO: renaming? */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token REQ VEQ RNEQ VNEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE ELSEIF BREAK CONTINUE FOR WHILE
%token INT BOOL VOID STRING CHAR FLOAT
%token ARROW CONCAT DEL DOT IN SPLICE APPEND TRY CATCH FINALLY
%token CLASS MAIN SELF NULL EXTENDS IMPLEMENTS CONST
%token <int> INT_LIT
%token <float> FLT_LIT
%token <string> STR_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE
%nonassoc ELSEIF
%nonassoc ELSE
%right ASSIGN
%left OR
%left AND
%left REQ RNEQ VEQ VNEQ
%left LT GT LEQ GEQ
%left PLUS MINUS MOD
%left TIMES DIVIDE
%right NOT NEG
%right DOT

%start program
%type <Ast.program> program

%%

program:
	cdecls EOF { $1 }

cdecls:
	/* TODO: allow empty program? */
	/* nothing */ { [], [] }
	cdecl_list	{ List.rev $1 }

cdecl_list:
	cdecl	{ [$1] }
	| cdecl_list cdecl	{ $2::$1 }

cdecl:
	CLASS ID LBRACE cbody RBRACE	{ {
		cname = $2
		cbody = $4 } }
	| CLASS ID EXTENDS ID LBRACE cbody RBRACE	{ {
		cname = $2
		sclass = $4
		cbody = $6 } }
	| CLASS ID IMPLEMENTS id_list LBRACE cbody RBRACE	{ {
		cname = $2
		interfaces = IdList($4)
		cbody = $6 } }
	| CLASS ID EXTENDS ID IMPLEMENTS id_list LBRACE cbody RBRACE
	{ {
		cname = $2
		sclass = $4
		interfaces = IdList($6)
		cbody = $8 } }

	CLASS ID LBRACE cbody RBRACE
	{ {
		cname = $2
		cbody = $4 } }

cbody:
	/* nothing */	{ {
		fields = [];
		methods = []; } }
	| cbody vdecl	{ {
		fields = $2 :: $1.fields;
		methods = $1.methods } }
	| cbody fdecl	{ {
		fields = $1.fields;
		methods = $2 :: $1.methods } }

fdecl:
	ID LPAREN formals_opt RPAREN ARROW typ LBRACE stmt_list RBRACE
	{ {
		typ = $6;
		fname = $1;
		formals = $3;
		locals = List.rev $8;
		body = List.rev $9 } }

formals_opt:
	  /* nothing */ { [] }
	| formal_list   { List.rev $1 }

formal_list:
	  typ ID                   { [Formal($1, $2)] }
	| formal_list COMMA typ ID { Formal($3, $4) :: $1 }

typ:
	  INT		{ Int }
	| BOOL	{ Bool }
	| VOID	{ Void }
	| STRING	{ String }
	| FLOAT	{ Float }
	/* TODO: char? Keep or remove? */
	/* TODO: put types in ast */
	/* Note: MUST forbid nested types, or allow (int)[] and such? (will cause
	 * shift/reduce conflicts if 2nd is allowed */
	/* TODO: object type, list, tuple needed */

/*
list_typ:
	typ LBRACK RBRACK	{ }

tuple_typ:
	typ LPAREN RPAREN	{ }
*/

vdecl_list:
	  /* nothing */    { [] }
	| vdecl_list vdecl { $2 :: $1 }

vdecl:
	  typ ID SEMI	{ ObjVar($1, $2) }
	| CONST typ ID SEMI	{ ObjConst($2, $3) }

id_list:
	  ID				{ Id($1) }
	| id_list COMMA ID	{ Id($1) }

stmt_list:
	  /* nothing */  { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI { Expr $1 }
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSE
		{ If($3, $6, Block([])) }
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE stmt_list
		{ If($3, Block(List.rev $6), Block(List.rev $9)) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
		{ For($3, $5, $7, Block(List.rev $10)) }
	| WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, $6) }
	| typ ID SEMI	{ LocalVar($1, $2) }
	| typ ID ASSIGN expr SEMI	{ LocalVar($1, $2, $4) }
	| CONST typ ID SEMI	{ LocalConst($2, $3) }
	| CONST typ ID ASSIGN expr SEMI	{ LocalConst($2, $3, $5) }

expr_opt:
	/* nothing */ { Noexpr }
	| expr          { $1 }

expr:
	  INT_LIT			{ IntLit($1) }
	| FLT_LIT			{ FltLit($1) }
	| STR_LIT			{ StrLit($1) }
	| TRUE				{ BoolLit(true) }
	| FALSE				{ BoolLit(false) }
	| ID				{ Id($1) }
	| expr PLUS   expr	{ Binop($1, Add,   $3) }
	| expr MINUS  expr	{ Binop($1, Sub,   $3) }
	| expr TIMES  expr	{ Binop($1, Mult,  $3) }
	| expr DIVIDE expr	{ Binop($1, Div,   $3) }
	| expr MOD	  expr	{ Binop($1, Mod,   $3) }
	| expr EQ     expr	{ Binop($1, Equal, $3) }
	| expr NEQ    expr	{ Binop($1, Neq,   $3) }
	| expr LT     expr	{ Binop($1, Less,  $3) }
	| expr LEQ    expr	{ Binop($1, Leq,   $3) }
	| expr GT     expr	{ Binop($1, Greater, $3) }
	| expr GEQ    expr	{ Binop($1, Geq,   $3) }
	| expr AND    expr	{ Binop($1, And,   $3) }
	| expr OR     expr	{ Binop($1, Or,    $3) }
	| MINUS expr %prec NEG	{ Unop(Neg, $2) }
	| NOT expr			{ Unop(Not, $2) }
	| ID ASSIGN expr	{ Assign($1, $3) }
	| ID LPAREN actuals_opt RPAREN	{ Call($1, $3) }
	| LPAREN expr RPAREN	{ $2 }

actuals_opt:
	  /* nothing */ { [] }
	| actuals_list  { List.rev $1 }

actuals_list:
	  expr                    { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
