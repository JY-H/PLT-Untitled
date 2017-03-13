/* Ocamlyacc parser for MicroC */

%{
open Ast
%}

%token SEMI LPAREN RPAREN LBRACE RBRACE LBRACK RBRACK COMMA
%token PLUS MINUS TIMES DIVIDE MOD ASSIGN NOT
%token REQ VEQ RNEQ VNEQ LT LEQ GT GEQ TRUE FALSE AND OR
%token RETURN IF ELSE ELSEIF BREAK CONTINUE FOR WHILE INT BOOL VOID STRING CHAR FLOAT
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
%left PLUS MINUS
%left TIMES DIVIDE
%right NOT NEG

%start program
%type <Ast.program> program

%%

program:
  decls EOF { $1 }

decls:
   /* nothing */ { [], [] }
 | decls vdecl { ($2 :: fst $1), snd $1 }
 | decls fdecl { fst $1, ($2 :: snd $1) }
 | decls cdecl { (* TODO: Add a third list for classes *) }

fdecl:
   ID LPAREN formals_opt RPAREN ARROW typ LBRACE vdecl_list stmt_list RBRACE
     { { typ = $6;
	 fname = $1;
	 formals = $3;
	 locals = List.rev $8;
	 body = List.rev $9 } }

formals_opt:
    /* nothing */ { [] }
  | formal_list   { List.rev $1 }

formal_list:
    typ ID                   { [($1,$2)] }
  | formal_list COMMA typ ID { ($3,$4) :: $1 }

typ:
    INT { Int }
  | BOOL { Bool }
  | VOID { Void }
  | STRING { String }

vdecl_list:
    /* nothing */    { [] }
  | vdecl_list vdecl { $2 :: $1 }

vdecl:
   typ ID SEMI { ($1, $2) }

cdecl_list:
    /* nothing */    { [] }
  | cdecl_list cdecl { $2 :: $1 }

cdecl:
    CLASS ID LBRACE vdecl_list stmt_list RBRACE
  | CLASS ID EXTENDS ID LBRACE vdecl_list stmt_list RBRACE
  | CLASS ID IMPLEMENTS id_list LBRACE vdecl_list stmt_list RBRACE
  | CLASS ID EXTENDS ID IMPLEMENTS id_list LBRACE vdecl_list stmt_list RBRACE

id_list:
    ID
  | id_list COMMA ID

stmt_list:
    /* nothing */  { [] }
  | stmt_list stmt { $2 :: $1 }

stmt:
    expr SEMI { Expr $1 }
  | RETURN SEMI { Return Noexpr }
  | RETURN expr SEMI { Return $2 }
  | LBRACE stmt_list RBRACE { Block(List.rev $2) }
  | IF LPAREN expr RPAREN stmt %prec NOELSE { If($3, $5, Block([])) }
  | IF LPAREN expr RPAREN stmt ELSE stmt    { If($3, $5, $7) }
  | FOR LPAREN expr_opt SEMI expr SEMI expr_opt RPAREN stmt
     { For($3, $5, $7, $9) }
  | WHILE LPAREN expr RPAREN stmt { While($3, $5) }

expr_opt:
    /* nothing */ { Noexpr }
  | expr          { $1 }

expr:
    LITERAL          { Literal($1) }
  | TRUE             { BoolLit(true) }
  | FALSE            { BoolLit(false) }
  | ID               { Id($1) }
  | expr PLUS   expr { Binop($1, Add,   $3) }
  | expr MINUS  expr { Binop($1, Sub,   $3) }
  | expr TIMES  expr { Binop($1, Mult,  $3) }
  | expr DIVIDE expr { Binop($1, Div,   $3) }
  | expr EQ     expr { Binop($1, Equal, $3) }
  | expr NEQ    expr { Binop($1, Neq,   $3) }
  | expr LT     expr { Binop($1, Less,  $3) }
  | expr LEQ    expr { Binop($1, Leq,   $3) }
  | expr GT     expr { Binop($1, Greater, $3) }
  | expr GEQ    expr { Binop($1, Geq,   $3) }
  | expr AND    expr { Binop($1, And,   $3) }
  | expr OR     expr { Binop($1, Or,    $3) }
  | MINUS expr %prec NEG { Unop(Neg, $2) }
  | NOT expr         { Unop(Not, $2) }
  | ID ASSIGN expr   { Assign($1, $3) }
  | ID LPAREN actuals_opt RPAREN { Call($1, $3) }
  | LPAREN expr RPAREN { $2 }

actuals_opt:
    /* nothing */ { [] }
  | actuals_list  { List.rev $1 }

actuals_list:
    expr                    { [$1] }
  | actuals_list COMMA expr { $3 :: $1 }
