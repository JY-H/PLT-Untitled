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
%token <int> INTLIT
%token <float> FLTLIT
%token <char> CHARLIT
%token <string> STRLIT
%token <string> ID
%token EOF

%nonassoc NOELSE NOELSEIF NOOBJECT
%nonassoc ELSEIF
%nonassoc ELSE
%nonassoc ID
%right ASSIGN
%left OR
%left AND
%left REQ RNEQ VEQ VNEQ
%left LT GT LEQ GEQ
%right DBLCOLON TILDE AMP
%left PLUS MINUS MOD
%left TIMES DIVIDE
%right NOT NEG
%left LPAREN
%left LBRACK
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
	ID LPAREN formals_opt RPAREN ARROW return_typ LBRACE stmt_list RBRACE
	{ {
		fname = $1;
		formals = $3;
		return_typ = $6;
		body = List.rev $8 } }

formals_opt:
	  /* nothing */ { [] }
	| formal_list   { List.rev $1 }

formal_list:
	  typ ID					{ [Formal($1, $2)] }
	| formal_list COMMA typ ID	{ Formal($3, $4) :: $1 }

typ:
	  primitive { $1 }
	| list_typ	{ $1 }
	| tuple_typ	{ $1 }
	/*| obj_typ	{ $1 }*/

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
	typ LBRACK RBRACK	{ Lst($1) }

tuple_typ:
	typ LPAREN RPAREN	{ Tuple($1) }

/*obj_typ:
	ID					{ Obj($1) }*/

id_list_opt:
	/* nothing */ { Some [] }
	| id_list     { Some (List.rev $1) }

id_list:
	  ID					{ [$1] } 
	| id_list COMMA ID	{ $3 :: $1 }

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
	| WHILE LPAREN expr RPAREN LBRACE stmt_list RBRACE { While($3,
		Block(List.rev $6)) }
	| BREAK	SEMI { Break }
	| CONTINUE SEMI { Continue }
	| typ ID SEMI	{ LocalVar($1, $2, Noexpr) }
	| typ ID ASSIGN expr SEMI	{ LocalVar($1, $2, $4) }
	| CONST primitive ID SEMI	{ LocalConst($2, $3, Noexpr) }
	| CONST primitive ID ASSIGN expr SEMI	{ LocalConst($2, $3, $5) }

elseifs:
	ELSEIF LPAREN expr RPAREN LBRACE stmt_list RBRACE %prec NOELSEIF
		{ Elseif($3, Block(List. rev $6)) :: [] }
	| ELSEIF LPAREN expr RPAREN LBRACE stmt_list RBRACE elseifs
		{ Elseif($3, Block(List.rev $6)) :: $8 }

expr_opt:
	/* nothing */ { Noexpr }
	| expr          { $1 }

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
	| MINUS expr %prec NEG	{ Unop(Neg, $2) }
	| NOT expr			{ Unop(Not, $2) }
	| TILDE expr		{ Unop(Remove, $2) }
	| LT typ GT expr	{ Cast($2, $4) }
	| expr ASSIGN expr	{ Assign($1, $3) }
	| expr DOT ID	{ FieldAccess($1, $3) }
	| expr DOT ID LPAREN actuals_opt RPAREN	{ MethodCall($1, $3, $5) }
	| LPAREN expr RPAREN	{ $2 }
	| sequence	{ $1 }
	| expr sequence_access	{ SeqAccess($1, fst $2, snd $2) }

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
	  LPAREN expr COMMA tuple_elems	{ TupleCreate(List.rev ($2 :: $4)) }
	| LBRACK list_elems		{ LstCreate(List.rev $2) }

tuple_elems:
	  RPAREN	{ [] }
	| expr RPAREN { [$1] }
	| expr COMMA tuple_elems { $1 :: $3 }

list_elems:
	  expr RBRACK	{ [$1] }
	| RBRACK	{ [] }
	| expr COMMA list_elems	{ $1 :: $3 }

sequence_access:
	  LBRACK expr RBRACK	{ ($2, Noexpr) }
	| LBRACK expr SNGCOLON expr RBRACK	{ ($2, $4) }

vdecl:
	  typ ID SEMI	{ ObjVar($1, $2, Noexpr) }
	| typ ID ASSIGN expr SEMI	{ ObjVar($1, $2, $4) }
	| CONST primitive ID SEMI	{ ObjConst($2, $3, Noexpr) }
	| CONST primitive ID ASSIGN expr SEMI	{ ObjConst($2, $3, $5) }

actuals_opt:
	  /* nothing */ { [] }
	| actuals_list  { List.rev $1 }

actuals_list:
	  expr                    { [$1] }
	| actuals_list COMMA expr { $3 :: $1 }
