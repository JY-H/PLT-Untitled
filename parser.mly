/* Ocamlyacc parser for DECAF */

%{ open Ast %}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN
%token REQ VEQ RNEQ VNEQ LT LEQ GT GEQ TRUE FALSE AND OR NOT
%token RETURN IF ELSE ELSEIF BREAK CONTINUE FOR WHILE THROW TRY CATCH FINALLY
%token INT BOOL VOID STRING CHAR FLOAT NULL
%token ARROW AMP TILDE DOT IN SNGCOLON DBLCOLON
%token CLASS SELF SUPER EXTENDS IMPLEMENTS CONST
%token <int> INTLIT
%token <float> FLTLIT
%token <char> CHARLIT
%token <string> STRLIT
%token <string> ID CLASSID
%token EOF

%nonassoc NOELSE NOFINALLY
%right ASSIGN
%left OR
%left AND
%left REQ RNEQ VEQ VNEQ
%left LT GT LEQ GEQ
%left IN
%right DBLCOLON TILDE AMP
%left PLUS MINUS MOD
%left TIMES DIVIDE
%right NOT NEG
%left LBRACK
%right DOT

%start program
%type <Ast.program> program

%%

program:
	global_decls EOF	{ Program($1) }

global_decls:
	  /* nothing */ { {
		cdecls = [];
		fdecls = []	} }
	| cdecl global_decls	{ { 
		cdecls = $1 :: $2.cdecls;
		fdecls = $2.fdecls	} }
	| fdecl global_decls	{ {
		cdecls = $2.cdecls;
		fdecls = $1 :: $2.fdecls } }

cdecl:
	/* TODO: possibly don't do interfaces because they'll be difficult */
	  CLASS CLASSID LBRACE cbody RBRACE	{ {
		cname = $2;
		cbody = $4;
		sclass = None;
		interfaces = None } }
	| CLASS CLASSID EXTENDS CLASSID LBRACE cbody RBRACE	{ {
		cname = $2;
		cbody = $6;
		sclass = Some $4;
		interfaces = None } }
	| CLASS CLASSID IMPLEMENTS classid_list_opt LBRACE cbody RBRACE	{ {
		cname = $2;
		cbody = $6;
		sclass = None;
		interfaces = $4 } }
	| CLASS CLASSID EXTENDS CLASSID IMPLEMENTS classid_list_opt LBRACE cbody
	  RBRACE { {
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
	ID LPAREN formals_opt RPAREN ARROW return_typ LBRACE stmt_list RBRACE
	{ {
		fname = $1;
		formals = $3;
		return_typ = $6;
		body = List.rev $8 } }
	| CLASSID LPAREN formals_opt RPAREN ARROW return_typ LBRACE stmt_list RBRACE
	{ {
		fname = $1;
		formals = $3;
		return_typ = $6;
		body = List.rev $8 } }

formals_opt:
	  /* nothing */	{ [] }
	| formal_list	{ List.rev $1 }

formal_list:
	  typ ID					{ [Formal($1, $2)] }
	| formal_list COMMA typ ID	{ Formal($3, $4) :: $1 }

typ:
	  primitive { $1 }
	| list_typ	{ $1 }
	| obj_typ	{ $1 }

return_typ:
	  VOID		{ Void }
	| typ		{ $1 }

primitive:
	  INT		{ Int }
	| CHAR		{ Char }
	| BOOL		{ Bool }
	| STRING	{ String }
	| FLOAT		{ Float }

list_typ:
	LBRACK typ RBRACK	{ ArrayTyp($2) }

obj_typ:
	CLASSID				{ ClassTyp($1) }

classid_list_opt:
	/* nothing */ { Some [] }
	| classid_list		{ Some (List.rev $1) }

classid_list:
	  CLASSID					{ [$1] } 
	| classid_list COMMA CLASSID	{ $3 :: $1 }

stmt_list:
	  /* nothing */  { [] }
	| stmt_list stmt { $2 :: $1 }

stmt:
	  expr SEMI { Expr $1 }
	| RETURN SEMI { Return Noexpr }
	| RETURN expr SEMI { Return $2 }
	/* if */
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSE
		{ If($3, Block(List.rev $6), [], Block([])) }
	/* if-else*/
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE ELSE LBRACE stmt_list RBRACE
		{ If($3, Block(List.rev $6), [],  Block(List.rev $10)) }
	/* if-elseif */
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE
	  elseifs
	  %prec NOELSE
		{ If($3, Block(List.rev $6), $8, Block([])) }
	/* if-elseif-else */
	| IF LPAREN expr RPAREN LBRACE stmt_list RBRACE
	  elseifs
	  ELSE LBRACE stmt_list RBRACE
		{ If($3, Block(List.rev $6), $8, Block(List.rev $11)) }
	| FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN LBRACE stmt_list RBRACE
		{ For($3, $5, $7, Block(List.rev $10)) }
	| WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE
		{ While($3, Block(List.rev $6)) }
	| BREAK	SEMI { Break }
	| CONTINUE SEMI { Continue }
	| TRY LBRACE stmt_list RBRACE catch_list %prec NOFINALLY
		{ TryCatch(Block(List.rev $3), $5, Block([])) }
	| TRY LBRACE stmt_list RBRACE catch_list FINALLY LBRACE stmt_list RBRACE
		{ TryCatch(Block(List.rev $3), $5, Block(List.rev $8)) }
	| THROW expr SEMI	{ Throw($2) }
	| typ ID SEMI	{ LocalVar($1, $2, Noexpr) }
	| typ ID ASSIGN expr SEMI	{ LocalVar($1, $2, $4) }
	| CONST primitive ID SEMI	{ LocalConst($2, $3, Noexpr) }
	| CONST primitive ID ASSIGN expr SEMI	{ LocalConst($2, $3, $5) }

catch_list:
	  CATCH LPAREN CLASSID ID RPAREN LBRACE stmt_list RBRACE
		{ [Catch(ClassTyp($3), $4, Block(List.rev $7))] }
	| CATCH LPAREN CLASSID ID RPAREN LBRACE stmt_list RBRACE catch_list
		{ Catch(ClassTyp($3), $4, Block(List.rev $7)) :: $9}

elseifs:
	  ELSEIF LPAREN expr RPAREN LBRACE stmt_list RBRACE
		{ [Elseif($3, Block(List.rev $6))] }
	| ELSEIF LPAREN expr RPAREN LBRACE stmt_list RBRACE elseifs
		{ Elseif($3, Block(List.rev $6)) :: $8 }

expr_opt:
	  /* nothing */	{ Noexpr }
	| expr			{ $1 }

expr:
	  lits				{ $1 }
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
	| expr DBLCOLON expr	{ Binop($1, Append, $3) }
	| expr AMP expr		{ Binop($1, Concat, $3) }
	| expr IN expr		{ Binop($1, In, $3) }
	| MINUS expr %prec NEG	{ Unop(Neg, $2) }
	| NOT expr			{ Unop(Not, $2) }
	| TILDE expr		{ Unop(Remove, $2) }
	| LT typ GT expr	{ Cast($2, $4) }
	| expr ASSIGN expr	{ Assign($1, $3) }
	| LPAREN expr RPAREN	{ $2 }
	| sequence	{ ArrayCreate(fst $1, snd $1) }
	| expr sequence_access	{ SeqAccess($1, $2) }
	| expr DOT ID	{ FieldAccess($1, Id($3)) }
	| ID LPAREN actuals_opt RPAREN	{ FuncCall($1, $3) }
	| expr DOT ID LPAREN actuals_opt RPAREN	{ MethodCall($1, $3, $5) }
	| obj_typ LPAREN actuals_opt RPAREN	{ ObjCreate($1, $3) }
	| SELF	{ Self }
	| SUPER LPAREN actuals_opt RPAREN	{ Super($3) }

lits:
	  INTLIT			{ IntLit($1) }
	| FLTLIT			{ FloatLit($1) }
	| CHARLIT			{ CharLit($1) }
	| STRLIT			{ StringLit($1) }
	| TRUE				{ BoolLit(true) }
	| FALSE				{ BoolLit(false) }
	| ID				{ Id($1) }
	| NULL				{ Null }

sequence:
	  LBRACK sequence COMMA expr RBRACK		{ (fst $2, $4 :: (snd $2)) }
	| LBRACK primitive COMMA expr RBRACK	{ ($2, [$4]) }
	| LBRACK obj_typ COMMA expr RBRACK		{ ($2, [$4]) }

sequence_access:
	  LBRACK expr RBRACK	{ $2 }

vdecl:
	  typ ID SEMI	{ ObjVar($1, $2, Noexpr) }
	| typ ID ASSIGN expr SEMI	{ ObjVar($1, $2, $4) }
	| CONST primitive ID SEMI	{ ObjConst($2, $3, Noexpr) }
	| CONST primitive ID ASSIGN expr SEMI	{ ObjConst($2, $3, $5) }

actuals_opt:
	  /* nothing */	{ [] }
	| actuals_list	{ List.rev $1 }

actuals_list:
	  expr						{ [$1] }
	| actuals_list COMMA expr	{ $3 :: $1 }
