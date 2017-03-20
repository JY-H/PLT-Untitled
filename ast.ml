(* Abstract Syntax Tree and functions for printing it *)

open Char

type op = Add | Sub | Mult | Div | Mod | Req | Veq | Rneq | Vneq | Less | Leq |
	Greater | Geq | And | Or | In | Append | Concat

type uop = Neg | Not | Remove

type typ = Int | Float | Bool | Char | String | Void |
	Tuple of typ | Lst of typ | Obj of string

(* typ ID, e.g. int x, int[] y *)
type formal_param = Formal of typ * string

type id_list = string list

type expr =
	  IntLit of int
	| BoolLit of bool
	| FloatLit of float
	| CharLit of char
	| StringLit of string
	| Id of string
	| Null
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of expr * expr
	| Cast of typ * expr
	| FieldAccess of expr * string
	| LstCreate of expr list
	| TupleCreate of expr list
	| SeqAccess of expr * expr * expr
	| MethodCall of expr * string * expr list
	| ObjCreate of typ * expr list
	| Self
	| Super of expr list
	| Noexpr

type stmt =
	  Block of stmt list
	| Expr of expr
	| Return of expr
	| If of expr * stmt * stmt
	| Elseif of expr * stmt
	| Elseifs of expr * stmt * stmt list * stmt
	| For of expr * expr * expr * stmt
	| While of expr * stmt
	| Break
	| Continue
	| LocalVar of typ * string * expr
	| LocalConst of typ * string * expr
	| TryCatch of stmt * stmt list * stmt
	| Catch of typ * expr * stmt

type field = ObjVar of typ * string * expr | ObjConst of typ * string * expr

type func_decl = {
	return_typ: typ;
	fname: string;
	formals: formal_param list;
	(* locals: local list; *)
	body: stmt list;
}

type class_body = {
	fields: field list;
	methods: func_decl list;
}

type class_decl = {
	cname: string;
	cbody: class_body;
	sclass: string option;
	interfaces: id_list option;
}

type global_decls = {
	cdecls: class_decl list;
	fdecls: func_decl list;
}

type program = Program of global_decls

(* Pretty-printing functions *)

let rec string_of_typ = function
	  Int -> "int"
	| Float -> "float"
	| Bool -> "bool"
	| String -> "string"
	| Void -> "void"
	| Char -> "char"
	| Tuple(t) -> "(" ^ string_of_typ t ^ ")"
	| Lst(t) -> "[" ^ string_of_typ t ^ "]"
	| Obj(id) -> id

let string_of_op = function
    Add -> "+"
  | Sub -> "-"
  | Mult -> "*"
  | Div -> "/"
  | Mod -> "%"
  | Req -> "=="
  | Veq -> "==="
  | Rneq -> "!="
  | Vneq -> "!=="
  | Less -> "<"
  | Leq -> "<="
  | Greater -> ">"
  | Geq -> ">="
  | And -> "and"
  | Or -> "or"
  | In -> "in"
  | Append -> "::"
  | Concat -> "@"

let string_of_uop = function
    Neg -> "-"
  | Not -> "not"
  | Remove -> "~"

let string_of_vdecl(t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec string_of_expr = function
	  IntLit(l) -> string_of_int l
	| FloatLit(f) -> string_of_float f
	| BoolLit(true) -> "true"
	| BoolLit(false) -> "false"
	| CharLit(c) -> Char.escaped c
	| StringLit(s) -> s
	| Id(s) -> s
	| Null -> "null"
	| Binop(e1, o, e2) ->
	    string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
	| Unop(o, e) -> string_of_uop o ^ string_of_expr e
	| Assign(e1, e2) -> string_of_expr e1 ^ " = " ^ string_of_expr e2
	| Cast(t, e) -> "<" ^ string_of_typ t ^ ">" ^ string_of_expr e
	| FieldAccess(obj, field) -> string_of_expr obj ^ "." ^ field
	| LstCreate(elems) -> "[" ^ String.concat ", " (List.map string_of_expr
		elems) ^ "]"
	| TupleCreate(elems) -> "(" ^ String.concat ", " (List.map string_of_expr
	  	elems) ^ ")"
	| SeqAccess(sequence, start_index, end_index) -> string_of_expr sequence ^
		"[" ^ string_of_expr start_index ^ (match end_index with
		  Noexpr -> ""
		| _ -> ": " ^ string_of_expr end_index)
		^ "]"
	| MethodCall(obj, f, el) ->
	     string_of_expr obj ^ "." ^ f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
	| ObjCreate(obj, args) ->
		string_of_typ obj ^ "(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
	| Self -> "self"
	| Super(args) -> "super(" ^ String.concat ", " (List.map string_of_expr args) ^ ")"
	| Noexpr -> ""

let rec string_of_stmt = function
	  Block(stmts) ->
		"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n"
	| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
	| If(if_expr, if_block , Block([])) ->
		"if (" ^ string_of_expr if_expr ^ ")\n" ^ string_of_stmt if_block
	| If(if_expr, if_block, else_block) ->
		"if (" ^ string_of_expr if_expr ^ ") " ^ string_of_stmt if_block ^
		"else " ^ string_of_stmt else_block
	| Elseif(e, block) ->
		"elseif (" ^ string_of_expr e ^ ") " ^ string_of_stmt block
	| Elseifs(if_expr, if_block, elseifs, else_block) ->
		"if (" ^ string_of_expr if_expr ^ ") " ^ string_of_stmt if_block ^
		String.concat "\n" (List.map string_of_stmt elseifs) ^
		"else " ^ string_of_stmt else_block
	| For(e1, e2, e3, block) ->
	    "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
		string_of_expr e3  ^ ") " ^ string_of_stmt block ^ "\n"
	| While(e, block) -> "while (" ^ string_of_expr e ^ ")" ^
		string_of_stmt block
	| Break -> "break;\n"
	| Continue -> "continue;\n"
	| LocalVar(t, id, e) -> string_of_typ t ^ " " ^ id ^ " " ^
		string_of_expr e ^ ";\n"
	| LocalConst(t, id, e) -> "const" ^ string_of_typ t ^ " " ^ id ^ " " ^
        string_of_expr e ^ ";\n"
	| TryCatch(try_block, catch_list, finally_block) ->
		"try " ^ string_of_stmt try_block ^
		String.concat "\n" (List.map string_of_stmt catch_list) ^
		"\nfinally " ^ string_of_stmt finally_block
	| Catch(t, id, block) -> "catch (" ^ string_of_typ t ^ " " ^
		string_of_expr id ^ " " ^ string_of_stmt block

let string_of_formal = function
	  Formal(t, name) -> string_of_typ t ^ " " ^ name

let string_of_field = function
	   ObjVar(t, name, e) ->
		string_of_typ t ^ " " ^ name ^ " = " ^ string_of_expr e ^ ";\n"
	 | ObjConst(t, name, e) ->
		"const" ^ string_of_typ t ^ " " ^ name ^ " = " ^ string_of_expr e ^ ";\n"

let string_of_func_decl func_decl =
	func_decl.fname ^ "(" ^
	String.concat ", " (List.map string_of_formal func_decl.formals) ^
	") ->" ^ string_of_typ func_decl.return_typ ^ " {\n" ^
	String.concat "" (List.map string_of_stmt func_decl.body) ^ "}\n"

let string_of_class_decl class_decl =
	"class " ^ class_decl.cname ^ " {\n" ^
	String.concat "" (List.map string_of_field class_decl.cbody.fields) ^ "\n" ^
	String.concat "\n" (List.map string_of_func_decl class_decl.cbody.methods) ^
	"\n}\n"

let string_of_global_decls gdecls = 
	String.concat "\n" (List.map string_of_class_decl gdecls.cdecls) ^
	String.concat "\n" (List.map string_of_func_decl gdecls.fdecls)

let string_of_program program = match program with
	Program gdecls -> string_of_global_decls gdecls
