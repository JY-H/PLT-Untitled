/* Ocamlyacc parser for DECAF */

%{ open Ast %}

/* TODO: renaming? */
%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token REQ VEQ RNEQ VNEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE ELSEIF BREAK CONTINUE FOR WHILE
%token INT BOOL VOID STRING CHAR FLOAT
%token ARROW AMP TILDE DOT IN SNGCOLON DBLCOLON TRY CATCH FINALLY
%token CLASS SELF NULL EXTENDS IMPLEMENTS CONST
%token <int> INT_LIT
%token <float> FLT_LIT
%token <string> STR_LIT
%token <string> ID
%token EOF

%nonassoc NOELSE NOELSEIF
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
	cdecls EOF { Program($1) }

cdecls:
	/* TODO: allow empty program? */
	  /* nothing */ { [] } 
        |  cdecl_list	{ List.rev $1 }

cdecl_list:
	cdecl	{ [$1] }
	| cdecl_list cdecl	{ $2::$1 }

cdecl:
        /* TODO: It may be worth separating out the classes that have
         * extensions/interfaces from those that don't. I'm a little wary of
         * about what I wrote here...
         */
	CLASS ID LBRACE cbody RBRACE	{ {
                cname = $2;
		cbody = $4;
                sclass = None;
                interfaces = None } }
	| CLASS ID EXTENDS ID LBRACE cbody RBRACE	{ {
                cname = $2;
                cbody = $6;
                sclass = Some $4;
                interfaces = None } }
	| CLASS ID IMPLEMENTS id_list_opt LBRACE cbody RBRACE	{ {
                cname = $2;
                cbody = $6;
                sclass = None;
                interfaces = $4 } }
	| CLASS ID EXTENDS ID IMPLEMENTS id_list_opt LBRACE cbody RBRACE
	{ {
                cname = $2;
                cbody = $8;
                sclass = Some $4;
                interfaces = $6 } }

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
		body = List.rev $8 } }

formals_opt:
	  /* nothing */ { [] }
	| formal_list   { List.rev $1 }

formal_list:
	  typ ID                   { [Formal($1, $2)] }
	| formal_list COMMA typ ID { Formal($3, $4) :: $1 }

typ:
	  INT		{ Int }
	| BOOL		{ Bool }
	| VOID		{ Void }
	| STRING	{ String }
	| FLOAT		{ Float }
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

id_list_opt:
        /* nothing */ { Some [] }
        | id_list     { Some (List.rev $1) }

/* Note: Inconsistency here. We don't actually have an Id defined in the AST. So
 * for now I'm just making this a list of strings. */
id_list:
        ID			{ [$1] } 
	| id_list COMMA ID	{ $3::$1 }

stmt_list:
	  /* nothing */  { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI { Expr $1 }
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	/* if */
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSE
		{ If($3, Block($6), Block([])) }
	/* if-elseif */
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE
	  elseifs
	  %prec NOELSE
		{ Elseifs($3, Block(List.rev $6), List.rev $8, Block([])) }
	/* if-elseif-else */
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE
	  elseifs
	  ELSE LBRACE stmt_list RBRACE
		{ Elseifs($3, Block(List.rev $6), List.rev $8,
		Block(List.rev $11)) }
	/* if-else*/
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
		{ If($3, Block(List.rev $6), Block(List.rev $10)) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
		{ For($3, $5, $7, Block(List.rev $10)) }
	| WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3, Block($6)) }
	| BREAK	SEMI { Break }
	| CONTINUE SEMI { Continue }
	| typ ID SEMI	{ LocalVar($1, $2, Noexpr) }
	| typ ID ASSIGN expr SEMI	{ LocalVar($1, $2, $4) }
	| CONST typ ID SEMI	{ LocalConst($2, $3, Noexpr) }
	| CONST typ ID ASSIGN expr SEMI	{ LocalConst($2, $3, $5) }

/* Note: likely to cause shift/reduce conflicts */
elseifs:
	ELSEIF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSEIF
		{ Elseif($3, Block(List. rev $6)) :: [] }
	| ELSEIF LPAREN expr RPAREN LBRACE stmt_list RBRACE elseifs
		{ Elseif($3, Block(List.rev $6)) :: $8 }

/* TODO: Allow variable declaration within for loops*/
expr_opt:
	/* nothing */ { Noexpr }
	| expr          { $1 }

expr:
	  INT_LIT			{ IntLit($1) }
	| FLT_LIT			{ FloatLit($1) }
	| STR_LIT			{ StringLit($1) }
	| TRUE				{ BoolLit(true) }
	| FALSE				{ BoolLit(false) }
	| ID				{ Id($1) }
	| expr PLUS expr	{ Binop($1, Add, $3) }
	| expr MINUS expr	{ Binop($1, Sub, $3) }
	| expr TIMES expr	{ Binop($1, Mult, $3) }
	| expr DIVIDE expr	{ Binop($1, Div, $3) }
	| expr MOD expr		{ Binop($1, Mod, $3) }
	| expr REQ expr		{ Binop($1, Req, $3) }
	| expr VEQ expr		{ Binop($1, Veq, $3) }
	| expr RNEQ expr	{ Binop($1, Rneq, $3) }
	| expr VNEQ expr	{ Binop($1, Vneq, $3) }
	| expr LT expr		{ Binop($1, Less, $3) }
	| expr LEQ expr		{ Binop($1, Leq, $3) }
	| expr GT expr		{ Binop($1, Greater, $3) }
	| expr GEQ expr		{ Binop($1, Geq, $3) }
	| expr AND expr		{ Binop($1, And, $3) }
	| expr OR expr		{ Binop($1, Or, $3) }
	| MINUS expr %prec NEG	{ Unop(Neg, $2) }
	| NOT expr			{ Unop(Not, $2) }
	| LT typ GT expr	{ Cast($2, $4) }
	| ID ASSIGN expr	{ Assign($1, $3) }
	| ID LPAREN actuals_opt RPAREN	{ Call($1, $3) }
	| LPAREN expr RPAREN	{ $2 }

vdecl:
	typ ID SEMI	{ ObjVar($1, $2, Noexpr) }
	| typ ID ASSIGN expr SEMI	{ ObjVar($1, $2, $4) }
	| CONST typ ID SEMI	{ ObjConst($2, $3, Noexpr) }
	| CONST typ ID ASSIGN expr SEMI	{ ObjConst($2, $3, $5) }

actuals_opt:
	  /* nothing */ { [] }
	| actuals_list  { List.rev $1 }

actuals_list:
	  expr                    { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
