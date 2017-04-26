(* Semantic checking for the DECAF compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

type class_map = {
	class_decl:		Ast.class_decl;
	class_methods:	Ast.func_decl StringMap.t;
	class_reserved_methods: Sast.sfunc_decl StringMap.t;
	class_fields:	Ast.field StringMap.t;
}

type env = {
	env_name:		string;
	env_locals:		typ StringMap.t;
	env_consts:		typ StringMap.t;
	env_params:		Ast.formal_param StringMap.t;
	env_ret_typ:	typ;
	env_reserved:	Sast.sfunc_decl list;
	env_class_maps:	class_map StringMap.t;
	(* Block of "block_name", local var map, and local const map *)
	env_blocks:		(string * (typ StringMap.t) * (typ StringMap.t)) list;
}

let update_env_name env name = {
	env_name = name;
	env_locals = env.env_locals;
	env_consts = env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = env.env_class_maps;
	env_blocks = env.env_blocks;
}

let add_empty_block env block_str = {
	env_name = env.env_name;
	env_locals = env.env_locals;
	env_consts = env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = env.env_class_maps;
	env_blocks = (block_str, StringMap.empty, StringMap.empty) :: env.env_blocks;
}

let update_env_class_maps env class_maps = {
	env_name = env.env_name;
	env_locals = env.env_locals;
	env_consts = env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = class_maps;
	env_blocks = env.env_blocks;
}

(* Add new local to env *)
let add_env_local env id typ = {
	env_name =env.env_name;
	env_locals = StringMap.add id typ env.env_locals;
	env_consts = env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = env.env_class_maps;
	env_blocks = env.env_blocks;
}

(* Add new const to env *)
let add_env_const env id typ = {
	env_name =env.env_name;
	env_locals = env.env_locals;
	env_consts = StringMap.add id typ env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = env.env_class_maps;
	env_blocks = env.env_blocks;
}

(* Add local var to current block *)
let add_env_block_local env id typ = {
	env_name = env.env_name;
	env_locals = env.env_locals;
	env_consts = env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = env.env_class_maps;
	env_blocks = match env.env_blocks with
		  [] -> raise(Failure("No env blocks found"))
		| head :: tail ->
			let block_str, local_map, const_map = head in
			let new_local_map = StringMap.add id typ local_map in
			(block_str, new_local_map, const_map) :: tail
}

(* Add local const to current block *)
let add_env_block_const env id typ = {
	env_name = env.env_name;
	env_locals = env.env_locals;
	env_consts = env.env_consts;
	env_params = env.env_params;
	env_ret_typ = env.env_ret_typ;
	env_reserved = env.env_reserved;
	env_class_maps = env.env_class_maps;
	env_blocks = match env.env_blocks with
		  [] -> raise(Failure("No env blocks found"))
		| head :: tail ->
			let block_str, local_map, const_map = head in
			let new_const_map = StringMap.add id typ const_map in
			(block_str, local_map, new_const_map) :: tail
}


let global_func_map_ref = ref StringMap.empty

let reserved_list =
(* Helper function to get all built-in functions. *)
	let reserved_struct name return_t formals =
		{
			stype = return_t;
			sfname = name;
			sformals = formals;
			sbody = [];
		}
	in
	let reserved = [
		reserved_struct "print" (Ast.Void) ([Formal(Ast.String, "string_arg")]);
	]
	in
	reserved

let reserved_map = List.fold_left (
			fun map f -> StringMap.add f.sfname f map
		) StringMap.empty reserved_list

(* Helper function to get fully qualified names so that we can distinguish
 * between global functions and class functions of the same name
 *)
let get_fully_qualified_name class_name fdecl = match fdecl.fname with
	  "main" -> "main"
	| _ ->	if class_name = "" then fdecl.fname else class_name ^ "." ^ fdecl.fname


(* Put all global function declarations into a map *)
(* if we keep reserved_map global, can rid of the second argument*)
let get_global_func_map fdecls reserved_map =
	let map_global_funcs map fdecl =
		if (StringMap.mem fdecl.fname map) then
			raise (Failure(" duplicate global function: " ^ fdecl.fname))
		else if (StringMap.mem fdecl.fname reserved_map) then
			raise (Failure(fdecl.fname ^ " is a reserved function."))
		else
			StringMap.add fdecl.fname fdecl map
	in
	List.fold_left map_global_funcs StringMap.empty fdecls


(* Pull all class internals into a map, including declaration, functions, and
 * fields.
 *)
let get_class_maps cdecls reserved_map =
	let map_class map cdecl =
		(* Map all fields, const and non-const. *)
		let map_fields map = function
			  ObjVar(_, name, _) as field ->
				if (StringMap.mem name map) then
					raise (Failure(" duplicate field name: " ^ name))
				else
					StringMap.add name field map
			| ObjConst(_, name, _) as const_field ->
				if (StringMap.mem name map) then
					raise (Failure(" duplicate const field name: " ^ name))
				else
					StringMap.add name const_field map
		in

		(* Map all functions within class declarations. *)
		let map_functions map fdecl =
			let func_full_name = get_fully_qualified_name cdecl.cname fdecl
			in
			if (StringMap.mem func_full_name map) then
				raise (Failure(" duplicate function: " ^ func_full_name))
			else if (StringMap.mem fdecl.fname reserved_map) then
				raise (Failure(fdecl.fname ^ " is a reserved function."))
			else
				StringMap.add func_full_name fdecl map
		in
		(* Map class names. *)
		(
		if (StringMap.mem cdecl.cname map) then
			raise (Failure(" duplicate class name: " ^ cdecl.cname))
		else
			StringMap.add cdecl.cname
			{
				class_fields = List.fold_left map_fields StringMap.empty
					cdecl.cbody.fields;
				class_methods = List.fold_left map_functions StringMap.empty
					cdecl.cbody.methods;
				class_reserved_methods = reserved_map;
				class_decl = cdecl;
			} map
		)
	in List.fold_left map_class StringMap.empty cdecls

let get_scdecl_from_cdecl sfdecls cdecl =
{
	scname = cdecl.cname;
	scbody = {
		sfields = cdecl.cbody.fields;
		smethods = sfdecls;
	}
}

let get_type_from_sexpr = function
	  SIntLit(_) -> Int
	| SBoolLit(_) -> Bool
	| SFloatLit(_) -> Float
	| SCharLit(_) -> Char
	| SStringLit(_) -> String
	| SId(_, t) -> t
	| SNull -> Null_t
	| SBinop(_, _, _, t) -> t
	| SUnop(_, _, t) -> t
	| SAssign(_, _, t) -> t
	| SCast(t, _) -> t
	| SFieldAccess(_, _, t) -> t
	| SCall(_, _, t) -> t
	| SObjCreate(t, _) -> t
	| SNoexpr -> Void

let rec get_sexprl env el =
	let env_ref = ref(env) in
	let rec helper = function
		hd :: tl -> let a_head, env = get_sexpr !env_ref hd in
			env_ref := env;
			a_head :: (helper tl)
		| [] -> []
	in (helper el), !env_ref

(* Check a binop is valid *)
and check_binop env expr1 op expr2 =
	let sexpr1, _ = get_sexpr env expr1 in
	let sexpr2, _ = get_sexpr env expr2 in

	let typ1 = get_type_from_sexpr sexpr1 in
	let typ2= get_type_from_sexpr sexpr2 in

	match op with
		  Add | Sub | Mult | Div | Mod ->
			check_arithmetic_op sexpr1 sexpr2 op typ1 typ2, env
		| Less | Leq | Greater | Geq ->
			check_relational_op sexpr1 sexpr2 op typ1 typ2, env
		| Veq | Vneq | Req | Rneq ->
			check_equality_op sexpr1 sexpr2 op typ1 typ2, env
		| And | Or ->
			check_logical_op sexpr1 sexpr2 op typ1 typ2, env
		(* TODO: list/tuple ops *)
		| _ -> raise(Failure("Operation " ^ string_of_op op ^
			" not supported."))

(* Check an arithmetic operation is valid *)
and check_arithmetic_op sexpr1 sexpr2 op typ1 typ2 = match typ1, typ2 with
	  Int, Int | Float, Float -> SBinop(sexpr1, op, sexpr2, typ1)
	(* NOTE: I didn't allow char ops but we could allow it *)
	| _ -> raise(Failure("Arithmetic operation not allowed between types " ^
		string_of_typ typ1 ^ " and " ^ string_of_typ typ2))

(* Check a relational operation is valid *)
and check_relational_op sexpr1 sexpr2 op typ1 typ2 = match typ1, typ2 with
	  Int, Int | Float, Float -> SBinop(sexpr1, op, sexpr2, Bool)
	| _ -> raise(Failure("Relational operation not allowed between types " ^
		string_of_typ typ1 ^ " and " ^ string_of_typ typ2))

(* Check an equality operation is valid *)
and check_equality_op sexpr1 sexpr2 op typ1 typ2 = match op with
	  Veq | Vneq -> (match typ1, typ2 with
		  Int, Int | Float, Float | Bool, Bool | Char, Char | String, String ->
			SBinop(sexpr1, op, sexpr2, Bool)
		| _ -> raise(Failure("Value equality/inequality operators cannot be " ^
			"used for types " ^
			string_of_typ typ1 ^ " and " ^ string_of_typ typ2)))
	| Req | Rneq -> (match typ1, typ2 with
			(* Superclasses (and classes in general) don't exist so this should
			be replaced*)
		  ClassTyp(classname1), ClassTyp(classname2) ->
			if classname1 = classname2 then
				SBinop(sexpr1, op, sexpr2, Bool)
			else
				raise(Failure("Unmatching class types used in referential " ^
				"equality/inequality operation"))
		| _ -> raise(Failure("Referential equality/inequality operators " ^
			"cannot be used for types " ^
			string_of_typ typ1 ^ " and " ^ string_of_typ typ2)))
	| _ -> raise(Failure("Equality operation not supported"))

(* Check a logical operation is valid *)
and check_logical_op sexpr1 sexpr2 op typ1 typ2 = match typ1, typ2 with
	  Bool, Bool -> SBinop(sexpr1, op, sexpr2, Bool)
	| _ -> raise(Failure("Boolean operation only allowed between two boolean" ^
		"expressions"))

and check_unop env op expr =
	let get_neg_op uop sexpr typ = match uop with
		  Neg -> SUnop(uop, sexpr, typ)
		| _ -> raise(Failure("Illegal unary operator applied to numeric type"))
	in
	let get_not_op uop sexpr typ = match uop with
		  Not -> SUnop(uop, sexpr, typ)
		| _ -> raise(Failure("Illegal unary operator applied to boolean type"))
	in

	let sexpr, _ = get_sexpr env expr in
	let typ = get_type_from_sexpr sexpr in
	match typ with
		  Int | Float -> get_neg_op op sexpr typ, env
		| Bool -> get_not_op op sexpr typ, env
		| _ -> raise(Failure("Unop can only be applied to numeric or boolean" ^
			"types"))

(* Check assignment, returns SAssign on success *)
and check_assign env expr1 expr2 =
	let sexpr1, env  = get_sexpr env expr1 in
	let sexpr2, env = get_sexpr env expr2 in

	let typ1 = get_type_from_sexpr sexpr1 in
	let typ2 = get_type_from_sexpr sexpr2 in

        match (typ1, sexpr2) with
                 ClassTyp(_), SNull -> SAssign(sexpr1, sexpr2, typ1), env
                | _ ->
	(* Only allow assignment lhs to be id or object field *)
	match sexpr1 with
		  SId(id, _) ->
			if StringMap.mem id env.env_consts then
				raise(Failure("Cannot assign to const id " ^ id))
			(* Check types match *)
			else if typ1 = typ2 then
				SAssign(sexpr1, sexpr2, typ2), env
			else
				raise(Failure("Cannot assign type " ^
					string_of_typ typ2 ^ " to type " ^ string_of_typ typ1))
		| SFieldAccess(_, sid, _) ->
			let id = match sid with
				  SId(id, _) -> id
				| _ -> raise(Failure("Object access only allowed on ids"))
			in
			(* Ensure id is not a const *)
			if StringMap.mem id env.env_consts then
				raise(Failure("Cannot assign to const id " ^ id))
			(* Check types match *)
			else if typ1 = typ2 then
				SAssign(sexpr1, sexpr2, typ2), env
			else
				raise(Failure("Cannot assign type " ^
					string_of_typ typ2 ^ " to type " ^ string_of_typ typ1))
		| _ -> raise(Failure("Invalid assignment: " ^
			string_of_expr expr1 ^ " = " ^ string_of_expr expr2))

(*func and method call*)
and check_func_call env fname el =
		let global_func_map = !global_func_map_ref
		in
	let sel, env = get_sexprl env el in
        let cmap =
            try StringMap.find env.env_name env.env_class_maps
            with | Not_found -> raise(Failure("Class undefined " ^ env.env_name))
        in
	let check_param formal param =
		let f_typ = match formal with Formal(t, _) -> t | _ -> Void in
		let p_typ = get_type_from_sexpr param in
		if (f_typ = p_typ) then param
		else
			raise(Failure("Incorrect type passed to function"))
			in
	let handle_params formals params =
				match formals, params with
						[], [] -> []
				|	   [], _ -> raise(Failure("Formals and actuals mismatch"))
				|	   _, [] -> raise(Failure("Cannot pass void as function params"))
				|	   _ ->
		let len1 = List.length formals in
		let len2 = List.length params in
		if len1 <> len2 then
			raise (Failure("Formal and actual argument numbers mismatch."))
		else
			List.map2 check_param formals sel
	in
	
	let full_name = env.env_name ^ "." ^ fname in
	try (
		let the_func = StringMap.find fname reserved_map in
		let actuals = handle_params the_func.sformals sel in
		SCall(fname, actuals, the_func.stype), env)
	with | Not_found ->
	try( 
		let the_func = StringMap.find fname global_func_map in
		let actuals = handle_params the_func.formals sel in
		SCall(fname, actuals, the_func.return_typ), env)
	with | Not_found ->
        try (
                let the_func = StringMap.find full_name cmap.class_methods in
                let actuals = handle_params the_func.formals sel in
                SCall(full_name, actuals, the_func.return_typ), env)
        with | Not_found ->
            raise (Failure("Function " ^ fname ^ " not found."))

(* Currently only casts primitives, and RHS must be a literal *)
and check_cast env to_typ expr =
	let sexpr, _ = get_sexpr env expr in
	let from_typ = get_type_from_sexpr sexpr in
	(* Check cast is valid from_typ -> to_typ *)
	let scast = match from_typ with
		  Bool -> (match to_typ with
			  Bool | Int | Float | String -> SCast(to_typ, sexpr)
			| _ -> raise(Failure("Cannot cast " ^ string_of_typ from_typ ^
				" to " ^ string_of_typ to_typ))
			)
		| Int -> (match to_typ with
			  Int | Bool | Float | String | Char -> SCast(to_typ, sexpr)
			| _ -> raise(Failure("Cannot cast " ^ string_of_typ from_typ ^
				" to " ^ string_of_typ to_typ))
			)
		| Float -> (match to_typ with
			  Float | Bool | Int | String  -> SCast(to_typ, sexpr)
			| _ -> raise(Failure("Cannot cast " ^ string_of_typ from_typ ^
				" to " ^ string_of_typ to_typ))
			)
		| _ -> raise(Failure("Cannot cast " ^ string_of_typ from_typ ^ " to " ^
			string_of_typ to_typ))
	in
	scast, env

and check_field_access env obj field =
	(* Find class exists *)
	let check_class_id expr = match expr with
		  Id(id) -> 
			let ctyp = get_id_typ env id in
			SId(id, ctyp)
		| Self -> SId("self", ClassTyp(env.env_name))
		| _ -> raise(Failure("No matching class found for id " ^ 
			string_of_expr expr))
	in

	let get_class_name obj = match obj with
		ClassTyp(name) -> name
		| _ -> raise(Failure("Expected object type"))
	in

	(* Find the matching field for this particular class *)
	let rec check_field lhs_env class_sid top_env expr =
		let class_name = get_class_name class_sid in
		match expr with
			Id(id) -> (
				try (
					let class_map = StringMap.find class_name
						lhs_env.env_class_maps in
					let match_field f = match f with
						ObjVar(dt, _, _) | ObjConst(dt, _, _) ->
							SId(id, dt), lhs_env
						| _ -> raise(Failure("Not a variable or const"))
					in
					match_field (StringMap.find id class_map.class_fields))
				with | Not_found -> raise(Failure("Unrecognized field")) )
			| FieldAccess(e1, e2) ->
				let old_env = lhs_env in
				let lhs, new_lhs_env = check_field lhs_env class_sid
					top_env e1 in
				let lhs_typ = get_type_from_sexpr lhs in
				let new_env = update_env_name new_lhs_env
					(get_class_name lhs_typ) in
				let rhs, _ = check_field new_env lhs_typ lhs_env e2 in
				let rhs_typ = get_type_from_sexpr rhs in
				SFieldAccess(lhs, rhs, rhs_typ), old_env
			| _ -> raise(Failure("Unrecognized datatype"))
	in
	let sexpr1 = check_class_id obj in
	let typ = get_type_from_sexpr sexpr1 in
	let obj_env = update_env_name env (get_class_name typ) in
	let sexpr2, _ = check_field obj_env typ env field in
	let field_type = get_type_from_sexpr sexpr2 in
	SFieldAccess(sexpr1, sexpr2, field_type), env
		(* Must be a class and id
		let check_field class_sid expr =
			match class_sid, expr with
			SId(classname, _), Id(id) ->
				let class_map = StringMap.find classname env.env_class_maps in
				if StringMap.mem id class_map.class_fields then
					let field = StringMap.find id class_map.class_fields in
					match field with
						ObjVar(ftyp, id, _) | ObjConst(ftyp, id, _) ->
							SId(id, ftyp)
				else
					raise(Failure("Unrecognized field in class " ^
					classname))
			| _ -> raise(Failure("Unrecognized data type") )
	in
	let field_expr = match field with
		  Id(id) -> Id(id)
		| _ -> raise(Failure("Field must be an id"))
	in
	let class_sid, _ = get_sexpr env obj in
	let field_sid = check_field class_sid field_expr in
	let field_type = get_type_from_sexpr field_sid in
	SFieldAccess(class_sid, field_sid, field_type), env*)

and check_object_create env t el =
    let s = string_of_typ t in
    let sel, env = get_sexprl env el in
    let cmap =
        try StringMap.find s env.env_class_maps
        with | Not_found -> raise(Failure("cannot construct undefined class object"))
    in
    (* currently don't support constructor overloading *)
    SObjCreate(t, sel), env

and get_sexpr env expr = match expr with
	  IntLit(i) -> SIntLit(i), env
	| BoolLit(b) -> SBoolLit(b), env
	| FloatLit(f) -> SFloatLit(f), env
	| CharLit(c) -> SCharLit(c), env
	| StringLit(s) -> SStringLit(s), env
	| Id(id) -> SId(id, (get_id_typ env id)), env
	| Null -> SNull, env
	| Binop(e1, op, e2) ->	check_binop env e1 op e2
	| Unop(op, e) -> check_unop env op e
	| Assign(e1, e2) -> check_assign env e1 e2
	| Cast(t, e) -> check_cast env t e
	| FieldAccess(classid, field) -> check_field_access env classid field
	| MethodCall(e, str, el) -> check_func_call env str el (*e is stored in env, but let's keep it that way and not bother to change ast*)
	| FuncCall(str, el) -> check_func_call env str el
	| ObjCreate(t, el) -> check_object_create env t el
	| Self -> SId("self", ClassTyp(env.env_name)), env
(*			  | Super*)
	| Noexpr -> SNoexpr, env
	| _ -> raise(Failure("cannot convert to sexpr")) (*dummy, delete later*)

(* Looks up the type of a variable/constant *)
and get_id_typ env id =
	(* Search block declarations *)
	let rec find_block_id id blocks = match blocks with
		  [] -> Void
		| head :: tail ->
			let _, local_map, const_map = head in
			if StringMap.mem id local_map then
				StringMap.find id local_map
			else if StringMap.mem id const_map then
				StringMap.find id const_map
			else
				find_block_id id tail
	in
	let block_typ = find_block_id id env.env_blocks in
	if block_typ != Void then
		block_typ
	else if StringMap.mem id env.env_locals then
		(* Found in function local declarations *)
		StringMap.find id env.env_locals
	else if StringMap.mem id env.env_consts then
		(* Found in function local declarations *)
		StringMap.find id env.env_consts
	else if StringMap.mem id env.env_params then
		(* Found in function parameters *)
		let param = StringMap.find id env.env_params in
		(function Formal(typ, _) -> typ) param
	else
	(* Note: Object fields are id's that are NOT handled by this currently *)
		(* Found a matching class name *)
		raise(Failure("Unknown identifier: " ^ id))

let rec check_block env stmtl = match stmtl with
	  [] -> SBlock([SExpr(SNoexpr, Void)]), env
	| _ ->
		let stmtl, _ = get_sstmtl env stmtl in
		SBlock(stmtl), env

and check_expr env expr =
	let sexpr, env = get_sexpr env expr in
	let styp = get_type_from_sexpr sexpr in
	SExpr(sexpr, styp), env

and check_return env expr =
	let sexpr, env = get_sexpr env expr in
	let styp = get_type_from_sexpr sexpr in
        match styp, env.env_ret_typ with
                 Null_t, ClassTyp(_) -> SReturn(sexpr, styp), env
                | _ ->
	if env.env_ret_typ = styp then
		SReturn(sexpr, styp), env
	else
		raise(Failure("Return type mismatch"))

and check_if env if_expr if_stmts elseifs else_stmts =
	let new_env = add_empty_block env "if" in
	let if_sexpr, if_env = get_sexpr new_env if_expr in
	let if_typ = get_type_from_sexpr if_sexpr in
	(* Check whether if expr is a bool *)
	let check_if_expr =
		if if_typ = Bool then
			()
		else
			raise(Failure("If condition must be a bool"))
	in
	let if_sstmts, _ = get_sstmt if_env if_stmts in
	(* Generate list of selseifs *)
	let rec get_selseifs elseifs = match elseifs with
		  [] -> []
		| head :: tail -> match head with
			  Elseif(elseif_expr, elseif_stmts) ->
				let elseif_sexpr, elseif_env = get_sexpr new_env elseif_expr in
				let elseif_typ = get_type_from_sexpr elseif_sexpr in
				let elseif_sstmts, _ = get_sstmt elseif_env elseif_stmts in
				if elseif_typ = Bool then
					SElseif(elseif_sexpr, elseif_sstmts) :: get_selseifs tail
				else
					raise(Failure("Elseif condition must be a bool"))
			| _ -> raise(Failure("Non-elseif found in elseif list"))
	in
	let selseifs = get_selseifs elseifs in
	let else_sstmts, _ = get_sstmt new_env else_stmts in
	SIf(if_sexpr, if_sstmts, selseifs, else_sstmts), env

and check_for env expr1 expr2 expr3 stmts =
	let new_env = add_empty_block env "for" in
	let sexpr1, _ = get_sexpr new_env expr1 in
	let sexpr2, _ = get_sexpr new_env expr2 in
	let sexpr3, _ = get_sexpr new_env expr3 in
	let sstmts, _ = get_sstmt new_env stmts in
	(* TODO: do we allow vampire loops, i.e. for(;;) *)
	SFor(sexpr1, sexpr2, sexpr3, sstmts), env

and check_while env expr stmts =
	let new_env = add_empty_block env "while" in
	let sexpr, new_env = get_sexpr new_env expr in
	let typ = get_type_from_sexpr sexpr in
	let sstmts, new_env = get_sstmt new_env stmts in
	match typ with
		  Bool | Void -> SWhile(sexpr, sstmts), env
		| _ -> raise(Failure("While condition only allows bool or void" ^
			" expression"))

and check_break env =
	(* Check break is in a loop body *)
	let rec find_loop blocks = match blocks with
		  [] -> false
		| head :: tail ->
			let block_str, _, _ = head in
			if block_str = "for" || block_str = "while" then
				true
			else
				find_loop tail
	in
	if find_loop env.env_blocks then
		SBreak, env
	else
		raise(Failure("Break can only be used within a loop"))

and check_continue env =
	(* Check continue is in a loop body *)
	let rec find_loop blocks = match blocks with
		  [] -> false
		| head :: tail ->
			let block_str, _, _ = head in
			if block_str = "for" || block_str = "while" then
				true
			else
				find_loop tail
	in
	if find_loop env.env_blocks then
		SContinue, env
	else
		raise(Failure("Continue can only be used within a loop"))


(* Verify local var type, add to local declarations *)
and check_local_var env typ id expr =
	if StringMap.mem id env.env_locals || StringMap.mem id env.env_consts then
		raise(Failure("Duplicate local declaration: " ^ id))
	else
		(* Look in block stack for duplicate declaration *)
		let rec search_block_stack id blocks = match blocks with
			  [] -> true
			| head :: tail ->
				let _, local_map, const_map = head in
				if StringMap.mem id local_map || StringMap.mem id const_map then
					raise(Failure("Duplicate block declaration: " ^ id))
				else
					search_block_stack id tail
		in
		let _ = search_block_stack id env.env_blocks in
		(* If in a block, add to local block declarations, else add to local
		declaration stack*)
		let env = match env.env_blocks with
			  [] -> add_env_local env id typ
			| _ -> add_env_block_local env id typ
		in
		let sexpr, env = get_sexpr env expr in
		(* If object type, check class name exists *)
		match typ with
			  ClassTyp(classname) ->
				if StringMap.mem classname env.env_class_maps then
					SLocalVar(typ, id, sexpr), env
				else
					raise(Failure("Unknown class type: " ^ classname))
			| _ -> (match sexpr with
				  SNoexpr -> SLocalVar(typ, id, sexpr), env
				| _ ->
					(* Check types match *)
					let typ_expr = get_type_from_sexpr sexpr in
					if typ = typ_expr then
						SLocalVar(typ, id, sexpr), env
					else
						raise(Failure("Declared type of " ^ id ^
						" and assignment type " ^ (string_of_typ) typ_expr ^
						" do not match"))
				)
				

(* Verify local const type and add to local declarations *)
and check_local_const env typ id expr =
	if StringMap.mem id env.env_locals || StringMap.mem id env.env_consts then
		raise(Failure("Duplicate local declaration: " ^ id))
	else
		(* Look in block stack for duplicate declaration *)
		let rec search_block_stack id blocks = match blocks with
			  [] -> true
			| head :: tail ->
				let _, local_map, const_map = head in
				if StringMap.mem id local_map || StringMap.mem id const_map then
					raise(Failure("Duplicate block declaration: " ^ id))
				else
					search_block_stack id tail
		in
		ignore(search_block_stack id env.env_blocks);

		(* If in a block, add to local block declarations, else add to local
		declaration stack*)
		let env = match env.env_blocks with
			  [] -> add_env_const env id typ
			| _ -> add_env_block_const env id typ
		in
		let sexpr, env = get_sexpr env expr in
		match sexpr with
			  SNoexpr ->
				SLocalConst(typ, id, sexpr), env
			| _ ->
				let typ_expr = get_type_from_sexpr sexpr in
				if typ = typ_expr then
					SLocalConst(typ, id, sexpr), env
				else
					raise(Failure("Declared type of " ^ id ^
					" and assignment type " ^ (string_of_typ) typ_expr ^
					" do not match"))

and get_sstmt env stmt = match stmt with
	  Block(blk) -> check_block env blk
	| Expr(expr) -> check_expr env expr
	| Return(expr) -> check_return env expr
	| If(if_expr, if_stmts, elseifs, else_stmts) ->
		check_if env if_expr if_stmts elseifs else_stmts
	| For(expr1, expr2, expr3, stmts) -> check_for env expr1 expr2 expr3 stmts
	| While(expr, stmts) -> check_while env expr stmts
	| Break -> check_break env
	| Continue -> check_continue env
	| LocalVar(typ, id, expr) -> check_local_var env typ id expr
	| LocalConst(typ, id, expr) -> check_local_const env typ id expr
(*			  | TryCatch
	|
	| Throw
	*)
	| _ -> raise(Failure("Idk what stmt you are talking abt"))

and get_sstmtl env stmtl =
	let env_ref = ref(env) in
	let rec helper = function
		head :: tail ->
			let sstmt, env = get_sstmt !env_ref head in
			env_ref := env;
			sstmt :: (helper tail)
		| [] -> []
	in
	let sstmts = (helper stmtl), !env_ref in sstmts

(* Check that a block of statements has some return statement in it *)
let rec check_block_return sblock = 
	let sstmts = match sblock with
		  SBlock(sstmts) -> sstmts
		| _ -> raise(Failure("Can only check SBlocks"))
	in
	let rec find_return sstmts = match sstmts with
		  [] -> false
		| head :: tail -> (match head with
			  SReturn(_, _) -> true
			| SIf(if_sexpr, if_sstmts, elseifs, else_sstmts) ->
				check_if_return if_sexpr if_sstmts elseifs else_sstmts
			| _ -> find_return tail
			)
	in
	find_return sstmts

(* Check whether an if-elseif-else block will definitely return *)
and check_if_return if_sexpr if_sstmts elseifs else_sstmts =
	let if_returns = check_block_return if_sstmts in
	let else_returns = check_block_return else_sstmts in
	if not if_returns || not else_returns then
		false
	else
		let rec check_elseifs_return elseifs = match elseifs with
			  [] -> true
			| head :: tail ->
				let selseif = (match head with
					  SElseif(sexpr, stmt) -> sexpr, stmt
					| _ -> raise(Failure("Non-elseif found in " ^
						"elseifs"))
					)
				in
				let _, elseif_sstmts = selseif in
				let elseif_returns = check_block_return elseif_sstmts in
				if not elseif_returns then
					false
				else
					check_elseifs_return tail
		in
		check_elseifs_return elseifs

(* return type is handled in check_return *)
let check_func_has_return fname sfbody return_typ =
	let len = List.length sfbody in
	if return_typ = Void then
		SExpr(SNoexpr, Void)
	else if len = 0 && return_typ != Void then
		raise(Failure("Cannot have void return type and empty function"))
	else
		(* Check if there is any return statement in func body *)
		let rec find_func_return sstmts = match sstmts with
			  [] -> false
			| head :: tail -> (match head with
				  SReturn(_, _) -> true
				| SIf(if_sexpr, if_sstmts, elseifs, else_sstmts) ->
					(* Check if there is an if-elseif-else block which is
					guaranteed to return *)
					let guaranteed_if_return =
					check_if_return if_sexpr if_sstmts elseifs else_sstmts
					in
					if guaranteed_if_return then
						true
					else
						find_func_return tail
				| _ -> find_func_return tail
				)
		in
		if find_func_return sfbody then
			SExpr(SNoexpr, Void)
		else
			raise(Failure("Missing return statement for a function that " ^
			"does not return void"))

let get_sfdecl_from_fdecl class_maps reserved cname fdecl =
        let class_formal = Formal(ClassTyp(cname), "self")
        in
	let get_params_map map formal = match formal with
		  Formal(_, name) -> StringMap.add name formal map
	in
	let parameters = 
            if cname = "" then List.fold_left get_params_map StringMap.empty (fdecl.formals)
            else List.fold_left get_params_map StringMap.empty (class_formal :: fdecl.formals) in
	let env = {
		env_name = cname;
		env_locals = StringMap.empty;
		env_consts = StringMap.empty;
		env_params = parameters;
		env_ret_typ = fdecl.return_typ;
		env_reserved = reserved;
		env_class_maps = class_maps;
		env_blocks = [];
	} in
	(* NOTE: tmp_env unused for now *)
	let func_sbody, _ = get_sstmtl env fdecl.body in
	let func_sbody = List.rev(check_func_has_return fdecl.fname func_sbody
	fdecl.return_typ :: List.rev(func_sbody)) in
	{
		stype = fdecl.return_typ;
		sfname = get_fully_qualified_name cname fdecl;
		sformals = class_formal :: fdecl.formals;
		sbody = func_sbody;
	}

(* Overview function to generate sast. We perform main checks here. *)
let get_sast class_maps reserved cdecls fdecls  =
	let find_main f = match f.sfname with
		  "main" -> true
		| _ -> false
	in
	let check_main functions =
		let all_main_decls = List.find_all find_main functions
		in
		if (List.length all_main_decls < 1 ) then
			raise (Failure("Main not defined."))
		else if (List.length all_main_decls  > 1) then
			raise (Failure("More than 1 main function defined."))
                else List.hd all_main_decls
	in
        (* later need to deal with overloading constructors
         * we dictate that each class (unlike java) must have a constructor*)
        let find_constructor scdecl =
            let cons_name = scdecl.scname ^ "." ^ "constructor"
            in
            let is_func_constructor f = (f.sfname = cons_name)
            in
            let scmethods = scdecl.scbody.smethods
            in
            try let _ = List.find is_func_constructor scmethods
            in
            scmethods
            with | Not_found -> raise(Failure("Constructor missing in class" ^ scdecl.scname))
        in
	let check_and_convert_cdecl cdecl =
		(*let class_map = StringMap.find cdecl.cname class_maps in*)
	        let sfunc_lst = List.fold_left (fun ls f ->
		(get_sfdecl_from_fdecl class_maps reserved cdecl.cname f) ::
			ls) [] cdecl.cbody.methods in
		let scdecl = get_scdecl_from_cdecl sfunc_lst cdecl in
                let constructor = find_constructor scdecl in
		(scdecl, sfunc_lst)
	in
        let iter_cdecls t c =
            let (scdecl, sfunc_lst) = check_and_convert_cdecl c in
            (scdecl :: fst t, sfunc_lst @ snd t)
        in
        let scdecl_lst, sfunc_lst = List.fold_left iter_cdecls ([], []) cdecls
        in
	let get_sfdecls l f =
		let sfdecl = (get_sfdecl_from_fdecl class_maps reserved "" f) in (sfdecl :: l)
	in
	let global_sfdecls = List.fold_left get_sfdecls [] fdecls
	in
	(* Check that there is one main function. *)
	let _ = check_main (global_sfdecls @ sfunc_lst)
	in
	{
		classes = scdecl_lst;
		functions = global_sfdecls;
		reserved = reserved
	}

let check program = match program with
	Program(globals) ->
		let global_func_map = get_global_func_map globals.fdecls reserved_map
		in global_func_map_ref := global_func_map;
		let class_maps = get_class_maps globals.cdecls reserved_map in
		let sast = get_sast class_maps reserved_list globals.cdecls globals.fdecls in
		sast
