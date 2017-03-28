(* Semantic checking for the DECAF compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

type class_map = {
	class_decl:		Ast.class_decl;
	class_methods:	Ast.func_decl StringMap.t;
	class_fields:	Ast.field StringMap.t;
}

type env = {
	env_name:		string;
	env_locals:		typ StringMap.t;
	env_params:		Ast.formal_param StringMap.t;
	env_ret_typ:	typ;
	env_reserved:	Sast.sfunc_decl list;
}

let update_env_name env name = {
	env_name =		name;
	env_locals =	env.env_locals;
	env_params = 	env.env_params;
	env_ret_typ =	env.env_ret_typ;
	env_reserved = 	env.env_reserved;
}

let global_func_map = StringMap.empty

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
    | _ ->  class_name ^ "." ^ fdecl.fname


(* Put all global function declarations into a map *)
(* if we keep reserved_map global, can rid of the second argument*)
let get_global_func_map fdecls reserved_map = 
    let map_global_funcs map fdecl = 
        if (StringMap.mem fdecl.fname global_func_map) then
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
            ObjVar(typ, name, e) as field ->
                if (StringMap.mem name map) then
                    raise (Failure(" duplicate field name: " ^ name))
                else
                    StringMap.add name field map
            | ObjConst(typ, name, e) as const_field ->
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
                class_decl = cdecl;
            } map
        )
    in List.fold_left map_class StringMap.empty cdecls

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
(*match el with
              [] -> []
            | head::tail -> get_sexpr head :: (get_sexprl tail)*)

and check_func_call env fname el =
    (*currently only support checks for built-in functions*)
    let sel, env = get_sexprl env el in
    let check_param formal param =
		let f_typ = match formal with Formal(t, _) -> t | _ -> Void in
		let p_typ = get_type_from_sexpr param in
		if (f_typ = p_typ) then param
		else
			raise(Failure("Incorrect type passed to function")) 
            in
	let handle_params (formals) params =
		(* Should do some formal/param pattern matching later *)
		(* Otherwise currently a simple length check *)
	    let len1 = List.length formals in
        let len2 = List.length params in
        if len1 <> len2 then
            raise (Failure("Formal and actual argument numbers mismatch."))
        else
            List.map2 check_param formals sel
	in
	
	(* TODO: Define full name
	let full_name = env.env_name ^ "." ^ fname in *)
	try (
		let the_func = StringMap.find fname reserved_map in
		let actuals = handle_params the_func.sformals sel in
		SCall(fname, actuals, the_func.stype) )
	with | Not_found ->
		raise (Failure("Function " ^ fname ^ " not found."))

and get_sexpr env expr = match expr with
              IntLit i -> SIntLit(i), env
            | BoolLit b -> SBoolLit(b), env
            | FloatLit f -> SFloatLit(f), env
            | CharLit c -> SCharLit(c), env
            | StringLit s -> SStringLit(s), env
            | Id s -> SId(s, Void), env
            | Null -> SNull, env
(*          to be implemented
 *          | Binop(e1, op, e2) ->  check_binop e1 op e2
            | Unop(op, e) -> check_uop op e
            | Assign(e1, e2) -> check_assign e1 e2
            | Cast(t, e) -> check_cast t e
            | FieldAccess(e, s) -> check_field_access e s
            | MethodCall*)
            | FuncCall(str, el) -> check_func_call env str el, env
(*            | ObjCreate*)
            | Self -> SId("self", Void), env (*void dummy*)
(*            | Super*)
            | Noexpr -> SNoexpr, env
            | _ -> raise(Failure("cannot convert to sexpr")) (*dummy, delete later*)

let rec check_block sl env = match sl with
	  [] -> SBlock([SExpr(SNoexpr, Void)])
	| _ ->
		let sl, _ = get_sstmtl env sl in
		SBlock(sl)

and check_expr e env =
    let se, env = get_sexpr env e in
    let t = get_type_from_sexpr se in
    SExpr(se, t)

and check_return e env =
    (*to do useful check we need some information about this function*)
    let se, env = get_sexpr env e in
    let t = get_type_from_sexpr se
    in SReturn(se, t)

and get_sstmt env stmt = match stmt with
              Block blk -> check_block blk env, env
            | Expr e -> check_expr e env, env
            | Return e -> check_return e env, env
(*            | If(e, s1, s2) -> check_if e s1 s2
            | For(e1, e2, e3, s) -> check_for e1 e2 e3 s
            | While(e, s) -> check_while e s
            | Break -> check_break
            | Continue -> check_continue
            | LocalVar(t, str, e) -> handle_local t str e
            | TryCatch
            | Catch
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
	(*(get_sstmt head) :: (helper tail)
    in helper stmtl*)

let get_sfdecl_from_fdecl reserved fdecl =
	let get_params_map map formal = match formal with
		Formal(typ, name) -> StringMap.add name formal map
		| _ -> map
	in
	let parameters = List.fold_left
		get_params_map StringMap.empty (fdecl.formals) in
	let env = {
		env_name = ""; (* Should be class name once classes happen *)
		env_locals = StringMap.empty;
		env_params = parameters;
		env_ret_typ = fdecl.return_typ;
		env_reserved = reserved;
	} in
    let func_sbody, tmp_env = get_sstmtl env fdecl.body
    in
    {
        stype = fdecl.return_typ;
        sfname = fdecl.fname;
        sformals = fdecl.formals;
        sbody = func_sbody;
    }

(* TODO: Things start screaming around here *)
(* Helper method to extract sfdecls from fdecls within classes. *)
let get_class_fdecls class_maps = 
    (* First use StringMap.fold to extract class decls from class_maps *)
    let class_fdecls = StringMap.fold (
        fun cname c l -> 
            (* Then extract fdecls within each cdecl and convert to sfdecl. *)
            let get_class_fnames result = StringMap.fold (
                fun fname f l -> (get_sfdecl_from_fdecl f)::l
            ) c.class_methods result
            in
            get_class_fnames l
    ) class_maps []
    in
    class_fdecls


(* Overview function to generate sast. We perform main checks here. *)
let get_sast class_maps global_func_maps reserved cdecls fdecls  =
    let find_main f = match f.sfname with 
        "main" -> true
        | _ -> false
    in
    let check_main functions = 
        let global_main_decls = List.find_all find_main functions
        in
        let class_main_decls = List.find_all find_main 
            (get_class_fdecls class_maps)
        in
        if ((List.length global_main_decls + List.length class_main_decls) < 1 ) then
            raise (Failure("Main not defined."))
        else if ((List.length global_main_decls + List.length class_main_decls) > 1) then
            raise (Failure("More than 1 main function defined."))
    in 
    let get_sfdecls l f =
        let sfdecl = get_sfdecl_from_fdecl f in sfdecl::l
    in
    let global_sfdecls = List.fold_left get_sfdecls [] fdecls
    in
    (* Check that there is one main function. *)
    let _ = check_main global_sfdecls
    in
    {
        classes = []; 
        functions = global_sfdecls;
        reserved = reserved 
    }

let check program = match program with
    Program(globals) ->  
        let global_func_map = get_global_func_map globals.fdecls reserved_map in
        let class_maps = get_class_maps globals.cdecls reserved_map in
        let sast = get_sast class_maps global_func_map reserved_list globals.cdecls globals.fdecls in
        sast
    (* TODO: A lot of shit. But check is the main function so we need to add top
     * level logic here. *)
