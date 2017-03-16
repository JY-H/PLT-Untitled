(* Ocamllex scanner for DECAF *)

{
	open Parser
	let unescape s =
		Scanf.sscanf("\"" ^ s ^ "\"") "%S%!" (fun x-> x)
}

let ascii = ([' '-'!' '#'-'[' ']'-'~'])
let escape = '\\' ['\\' ''' '"' 'n' 'r' 't']

rule token = parse
	  [' ' '\t' '\r' '\n'] { token lexbuf } (* Whitespace *)
	| "/*"		{ comment lexbuf }           (* Comments *)
	| "//"		{ singleLineComment lexbuf }
	| '('		{ LPAREN }
	| ')'		{ RPAREN }
	| '{'		{ LBRACE }
	| '}'		{ RBRACE }
	| '['		{ LBRACK }
	| ']'		{ RBRACK }
	| ';'		{ SEMI }
	| ','		{ COMMA }
	| '+'		{ PLUS }
	| '-'		{ MINUS }
	| '*'		{ TIMES }
	| '/'		{ DIVIDE }
	| '%'		{ MOD }
	| '=' 		{ ASSIGN }
	| "=="		{ REQ }
	| "==="		{ VEQ }
	| "!="		{ RNEQ }
	| "!=="		{ VNEQ }
	(* TODO: resolve LT vs LANGLE and GT vs RANGLE*)
	| '<'		{ LT }
	| "<="		{ LEQ }
	| '>'		{ GT }
	| ">="		{ GEQ }
	| "and"		{ AND }
	| "or"		{ OR }
	| "not"		{ NOT }
	| "if"		{ IF }
	| "else"	{ ELSE }
	| "elseif"	{ ELSEIF }
	| "break"	{ BREAK }
	| "continue"	{ CONTINUE }
	| "for"		{ FOR }
	| "while"	{ WHILE }
	| "return"	{ RETURN }
	| "int"		{ INT }
	| "bool" 	{ BOOL }
	| "void"	{ VOID }
	| "string"	{ STRING }
	| "char"	{ CHAR }
	| "float"	{ FLOAT }
	| "true"	{ TRUE }
	| "false"	{ FALSE }
	| "->"		{ ARROW }
	(* rename to AMP? *)
	| "@"		{ CONCAT }
	(* rename to TILDE? *)
	| "~" 		{ DEL }
	| "."		{ DOT }
	| "in"		{ IN }
	(* TODO: rename to SNGCOLON *)
	| ":"		{ SPLICE }
	(* TODO: rename to DBLCOLON*)
	| "::"		{ APPEND }
	| "try"		{ TRY }
	| "catch"	{ CATCH }
	| "finally"	{ FINALLY }
	| "class"	{ CLASS }
	| "main"	{ MAIN }
	| "self"	{ SELF }
	| "null"	{ NULL }
	| "extends"	{ EXTENDS }
	| "implements"	{ IMPLEMENTS }
	| "const"	{ CONST }
	| ['0'-'9']+ as lxm { INT_LIT(int_of_string lxm) }
	| ['0'-'9']+'.'['0'-'9']+ as lxm { FLT_LIT(float_of_string lxm) }
	| ['a'-'z' 'A'-'Z']['a'-'z' 'A'-'Z' '0'-'9' '_']* as lxm { ID(lxm) }
	| '"'((ascii|escape))*'"' as s { STR_LIT(unescape s) }
	| eof { EOF }
	| _ as char { raise (Failure("illegal character " ^ Char.escaped char)) }
	
and comment = parse
  "*/" { token lexbuf }
| _    { comment lexbuf }

and singleLineComment = parse
 '\n'	{ token lexbuf }
| _		{ singleComment lexbuf }
