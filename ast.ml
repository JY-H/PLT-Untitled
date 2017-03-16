(* Abstract Syntax Tree and functions for printing it *)

type op = Add | Sub | Mult | Div | Mod | Req | Veq | Rneq | Vneq | Less | Leq |
	Greater | Geq | And | Or

type uop = Neg | Not

type typ = Int | Float | Bool | String | Void (* | Tuple of typ | List of typ |
	Obj of ObjType *)

(* typ ID, e.g. int x, int[] y *)
type formal_param = Formal of typ * string

type field = ObjVar of typ * string | ObjConst of typ * string

type local = LocalVar of typ * string | LocalConst of typ * string

type id_list = string list

type expr =
	  IntLit of int
	| BoolLit of bool
	| FloatLit of float
	| StringLit of string
	| Id of string
	| Binop of expr * op * expr
	| Unop of uop * expr
	| Assign of string * expr
	| Cast of typ * expr
	| Call of string * expr list
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
	| LocalVar of typ * string
	| LocalConst of typ * string


type func_decl = {
    typ: typ;
    fname: string;
    formals: formal_param list;
    locals: local list;
    body: stmt list;
}

type class_body = {
	fields: field list;
	methods: func_decl list;
}

type class_decl = {
	cname: string;
	cbody: class_body;
}

type program = Program of class_decl list

(* Pretty-printing functions *)

let string_of_typ = function
	  Int -> "int"
	| Float -> "float"
	| Bool -> "bool"
	| String -> "string"
	| Void -> "void"

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
  | And -> "&&"
  | Or -> "||"

let string_of_uop = function
    Neg -> "-"
  | Not -> "not "

let string_of_vdecl(t, id) = string_of_typ t ^ " " ^ id ^ ";\n"

let rec string_of_expr = function
    IntLit(l) -> string_of_int l
  | FloatLit(f) -> string_of_float f
  | BoolLit(true) -> "true"
  | BoolLit(false) -> "false"
  | StringLit(s) -> s
  | Id(s) -> s
  | Binop(e1, o, e2) ->
      string_of_expr e1 ^ " " ^ string_of_op o ^ " " ^ string_of_expr e2
  | Unop(o, e) -> string_of_uop o ^ string_of_expr e
  | Assign(v, e) -> v ^ " = " ^ string_of_expr e
  | Cast(t, e) -> "<" ^ string_of_typ t ^ ">" ^ string_of_expr e
  | Call(f, el) ->
      f ^ "(" ^ String.concat ", " (List.map string_of_expr el) ^ ")"
  | Noexpr -> ""

let rec string_of_stmt = function
	  Block(stmts) ->
		"{\n" ^ String.concat "" (List.map string_of_stmt stmts) ^ "}\n"
	| Expr(expr) -> string_of_expr expr ^ ";\n"
	| Return(expr) -> "return " ^ string_of_expr expr ^ ";\n"
	| If(e, s, Block([])) -> "if (" ^ string_of_expr e ^ ")\n" ^
		string_of_stmt s
	| If(e, s1, s2) ->  "if (" ^ string_of_expr e ^ ") {\n" ^
		string_of_stmt s1 ^ "\n}\nelse {\n" ^ string_of_stmt s2 ^ "\n}\n"
	| Elseif(e, s) -> "elseif (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s
		^ "\n}\n"
	| Elseifs(if_expr, if_stmt, elseifs, else_stmt) -> ""
		(*"if (" ^ string_of_expr if_expr ^ ") {\n" ^ string_of_stmt if_stmt ^
		"\n}\n" ^ String.concat "\n" (List.map
			(function Elseif(expr, block) -> "elseif (" ^
			string_of_expr expr ^ ") {\n" ^ string_of_stmt block ^ "\n}")
		elseifs) ^
		"else {\n" ^ string_of_stmt else_stmt ^ "\n}\n"*)
	| For(e1, e2, e3, s) ->
	    "for (" ^ string_of_expr e1  ^ " ; " ^ string_of_expr e2 ^ " ; " ^
		string_of_expr e3  ^ ") {\n" ^ string_of_stmt s ^ "\n}\n"
	| While(e, s) -> "while (" ^ string_of_expr e ^ ") {\n" ^ string_of_stmt s ^
		"\n}\n"
	| Break -> ""
	| Continue -> ""
	| LocalVar(t, id) -> string_of_typ t ^ " " ^ id ^ ";\n"
	| LocalConst(t, id) -> "const" ^ string_of_typ t ^ " " ^ id ^ ";\n"

let string_of_formal = function
	  Formal(t, name) -> string_of_typ t ^ " " ^ name

let string_of_field = function
	   ObjVar(t, name) -> string_of_typ t ^ " " ^ name
	 | ObjConst(t, name) -> "const" ^ string_of_typ t ^ " " ^ name

let string_of_fdecl func_decl =
	func_decl.fname ^ "(" ^
	String.concat ", " (List.map string_of_formal func_decl.formals) ^
	") ->" ^ string_of_typ func_decl.typ ^ " {\n" ^
	String.concat "" (List.map string_of_stmt func_decl.body) ^ "}\n"

let string_of_class class_decl =
	"class " ^ class_decl.cname ^ " {\n" ^
	String.concat "" (List.map string_of_field class_decl.cbody.fields) ^ "\n" ^
	String.concat "\n" (List.map string_of_fdecl class_decl.cbody.methods) ^
	"\n}\n"

let string_of_program program = match program with
	Program cdecls -> String.concat "" (List.map string_of_class cdecls)
