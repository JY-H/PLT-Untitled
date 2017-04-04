(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)
open Llvm
open Ast
open Sast
open Semant

module L = Llvm
module A = Ast (* I would like to get rid of these (now that we opened these modules, we can directly use them anyways), but have not made any changes yet in case someone feel strongly against it *)
module S = Sast

module Hash = Hashtbl

module StringMap = Map.Make(String)
let global_classes:(string, L.lltype) Hash.t = Hash.create 50
let local_params:(string, L.llvalue) Hash.t = Hash.create 50
let local_values:(string, L.llvalue) Hash.t = Hash.create 50

let context = L.global_context()
let codegen_module = L.create_module context "DECAF Codegen"
let builder = L.builder context

let i1_t = L.i1_type context;;
let i8_t = L.i8_type context;;
let i32_t = L.i32_type context;;
let f_t = L.double_type context;;
let str_t = L.pointer_type i8_t;;
let void_t = L.void_type context;;

let rec get_llvm_type = function
	  A.Int -> i32_t
	| A.Float -> f_t
	| A.Bool -> i1_t
	| A.Char -> i8_t
	| A.Void -> void_t
	| A.String -> str_t
	(* TODO: Add tuple/list types *)
	| A.Obj(name) -> L.pointer_type(find_global_class name)
	| _ -> raise(Failure("Type not yet supported."))

and find_global_class name =
	try Hash.find global_classes name
	with Not_found -> raise(Failure("Invalid class name."))	 

(* Truthfully wtf am I doing I'm a monkey *)
let rec id_gen llbuilder id is_deref =
	if is_deref then
		try
			let _val = Hash.find local_params id in
			L.build_load _val id llbuilder
		with Not_found ->
		try
			let _val = Hash.find local_values id in
			L.build_load _val id llbuilder
		with Not_found ->
			raise(Failure("Unknown variable " ^ id))
	else
		try
			Hash.find local_values id
		with Not_found ->
		try
			Hash.find local_params id
		with Not_found ->
			raise(Failure("Unknown variable " ^ id))

and func_lookup fname = match (L.lookup_function fname codegen_module) with
	  None -> raise(Failure(" function " ^ fname ^ " does not exist."))
	| Some func -> func 

and string_gen llbuilder s =
	L.build_global_stringptr s "tmp" llbuilder

(*
 * Additionally, this isn't the full case coverage, so we need to add that as
 * well.
 *)
and sstmt_gen llbuilder = function
	  SBlock st -> List.hd(List.map (sstmt_gen llbuilder) st)
	| SExpr(sexpr, _) -> sexpr_gen llbuilder sexpr
	| SIf(if_expr, if_stmts, elseifs, else_stmts) ->
		if_gen llbuilder if_expr if_stmts elseifs else_stmts
	| SLocalVar(typ, id, sexpr) -> ignore(local_var_gen llbuilder typ id);
		assign_gen llbuilder (SId(id, typ)) sexpr typ
	| _ -> raise(Failure("Unknown statement reached."))

and sexpr_gen llbuilder = function
	  SIntLit(i) -> L.const_int i32_t i
	| SBoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
	| SFloatLit(f) -> L.const_float f_t f
	| SStringLit(s) -> string_gen llbuilder s
	| SId(id, typ) -> id_gen llbuilder id true
	| SBinop(sexpr1, op, sexpr2, typ) ->
		binop_gen llbuilder sexpr1 op sexpr2 typ
	| SUnop(op, sexpr, typ) ->
		unop_gen llbuilder op sexpr typ
	| SAssign(sexpr1, sexpr2, typ) -> assign_gen llbuilder sexpr1 sexpr2 typ
	| SCall("print", sexpr_list, typ) -> call_gen llbuilder "printf" sexpr_list
		A.Void
	| SNoexpr -> L.const_int i32_t 0
	| _ -> raise(Failure("Expression type not recognized.")) 

and binop_gen llbuilder sexpr1 op sexpr2 typ =
	let lexpr1 = sexpr_gen llbuilder sexpr1 in
	let lexpr2 = sexpr_gen llbuilder sexpr2 in

	let typ1 = get_type_from_sexpr sexpr1 in
	let typ2 = get_type_from_sexpr sexpr2 in

	let int_ops expr1 binop expr2 = match binop with
		  Add -> L.build_add expr1 expr2 "int_addtmp" llbuilder
		| Sub -> L.build_sub expr1 expr2 "int_subtmp" llbuilder
		| Mult -> L.build_mul expr1 expr2 "int_multop" llbuilder
		| Div -> L.build_sdiv expr1 expr2 "int_divop" llbuilder
		| Mod -> L.build_srem expr1 expr2 "int_modop" llbuilder
		| Veq -> L.build_icmp L.Icmp.Eq expr1 expr2 "int_eqtmp" llbuilder
		| Vneq -> L.build_icmp L.Icmp.Ne expr1 expr2 "int_neqtmp" llbuilder
		| Less -> L.build_icmp L.Icmp.Slt expr1 expr2 "int_lesstmp" llbuilder
		| Leq -> L.build_icmp L.Icmp.Sle expr1 expr2 "int_leqtmp" llbuilder
		| Greater -> L.build_icmp L.Icmp.Sgt expr1 expr2 "int_greatertmp" llbuilder
		| Geq -> L.build_icmp L.Icmp.Sge expr1 expr2 "int_geqtmp" llbuilder
		| _ -> raise(Failure("Unsupported operator for integers"))
	in
	
	let float_ops expr1 binop expr2 = match binop with
		  Add -> L.build_fadd expr1 expr2 "flt_addtmp" llbuilder
		| Sub -> L.build_fsub expr1 expr2 "flt_subtmp" llbuilder
		| Mult -> L.build_fmul expr1 expr2 "flt_multop" llbuilder
		| Div -> L.build_fdiv expr1 expr2 "flt_divop" llbuilder
		| Mod -> L.build_frem expr1 expr2 "flt_modop" llbuilder
		| Veq -> L.build_fcmp L.Fcmp.Oeq expr1 expr2 "flt_eqtmp" llbuilder
		| Vneq -> L.build_fcmp L.Fcmp.One expr1 expr2 "flt_neqtmp" llbuilder
		| Less -> L.build_fcmp L.Fcmp.Ult expr1 expr2 "flt_lesstmp" llbuilder
		| Leq -> L.build_fcmp L.Fcmp.Ole expr1 expr2 "flt_leqtmp" llbuilder
		| Greater -> L.build_fcmp L.Fcmp.Ogt expr1 expr2 "flt_greatertmp" llbuilder
		| Geq -> L.build_fcmp L.Fcmp.Oge expr1 expr2 "flt_geqtmp" llbuilder
		| _ -> raise(Failure("Unsupported operator for floats"))
	in

	(* TODO: do something for req/rneq here *)
	
	match typ with
		  Int | Bool -> int_ops lexpr1 op lexpr2
		| Float -> float_ops lexpr1 op lexpr2
		| _ -> raise(Failure("Unrecognized data type in binop"))

and unop_gen llbuilder unop sexpr typ =
	let unop_lval = sexpr_gen llbuilder sexpr in
	
	let build_unop op unop_typ lval = match op, unop_typ with
		  Neg, Int -> L.build_neg lval "neg_int_tmp" llbuilder
		| Neg, Float -> L.build_fneg lval "neg_flt_tmp" llbuilder
		| Not, Bool -> L.build_not lval "not_bool_tmp" llbuilder
		| _ -> raise(Failure("Unsupported unop for " ^ string_of_uop op ^ 
			" and type " ^ string_of_typ typ))
	in

	match typ with
		  Int | Float | Bool -> build_unop unop typ unop_lval
		| _ -> raise(Failure("Invalid type for unop: " ^ string_of_typ typ))

(* Assignment instruction generation *)
and assign_gen llbuilder sexpr1 sexpr2 typ =
	let rhs_typ = get_type_from_sexpr sexpr2 in

	let lhs, is_obj_access = match sexpr1 with
		  SId(id, typ) -> id_gen llbuilder id false, false
		(* TODO: add functionality  for objects, tuples, etc. *)
		(*| SFieldAccess(id, field, typ)) -> *)
		| _ -> raise(Failure("Unable to assign."))
	in

	let rhs = match sexpr2 with
		  SId(id, typ) -> (match typ with
			  Obj(classname) -> id_gen llbuilder id false
			| _ -> id_gen llbuilder id true)
		(* TODO: implement when field access allowed *)
		(*| SFieldAccess(id, field, typ) ->*)
		| _ -> sexpr_gen llbuilder sexpr2
	in

	let rhs = match typ with
		  Obj(classname) ->
			if is_obj_access then
				rhs
			else
				L.build_load rhs "tmp" llbuilder
		| _ -> rhs
	in

	ignore(L.build_store rhs lhs llbuilder);
	rhs


(* Same with call_gen. Need to add full case coverage. *)
and call_gen llbuilder func_name sexpr_list stype =
	match func_name with
	  "printf" -> print_gen llbuilder sexpr_list
	| _ -> raise(Failure("CALL_GEN -- Not yet supported, go write it yourself."))


(* Helper method to generate printf function for strings. *)
and print_gen llbuilder sexpr_list =
	let params = List.map (fun expr -> sexpr_gen llbuilder expr) sexpr_list
	in
	L.build_call (func_lookup "printf") 
		(Array.of_list ((sexpr_gen llbuilder (SStringLit("%s")))::params))
		"printf" llbuilder

(* prepare your butt *)
and if_gen llbuilder if_sexpr if_sstmts elseifs else_sstmts =
	(* if expr *)
	let if_lexpr = sexpr_gen llbuilder if_sexpr in

	(* Initial bb *)
	let if_start_bb = L.insertion_block llbuilder in
	let parent_func = L.block_parent if_start_bb in

	(* if bb *)
	let if_bb = L.append_block context "then" parent_func in

	(* Create if bb statements *)
	L.position_at_end if_bb llbuilder;
	ignore(sstmt_gen llbuilder if_sstmts);

	let new_if_bb = L.insertion_block llbuilder in

	(* elseif bbs *)
	let rec make_elseif_bbs elseifs = match elseifs with
		  [] -> []
		| head :: tail ->
			let selseif = match head with
				  SElseif(sexpr, sstmt) -> (sexpr, sstmt)
				| _ -> raise(Failure("Unexpected non-elseif in elseif list"))
			in
			let elseif_sexpr, elseif_sstmts = selseif in

			(* elseif expr *)
			let elseif_lexpr = sexpr_gen llbuilder elseif_sexpr in

			let elseif_start_bb = L.insertion_block llbuilder in

			(* elseif bb *)
			let elseif_bb = L.append_block context "elseif" parent_func in
			L.position_at_end elseif_bb llbuilder;

			(* Create elseif bb statements *)
			ignore(sstmt_gen llbuilder elseif_sstmts);
			let new_elseif_bb = L.insertion_block llbuilder in
			(elseif_lexpr, elseif_start_bb, elseif_bb, new_elseif_bb) :: 
				make_elseif_bbs tail
	in
	let elseif_bbs = make_elseif_bbs elseifs in
	let if_elseif_bbs = (if_lexpr, if_start_bb, if_bb, new_if_bb) ::
	elseif_bbs in
	
	(* else bb *)
	let else_bb = L.append_block context "else" parent_func in

	(* Create else bb statements *)
	L.position_at_end else_bb llbuilder;
	ignore(sstmt_gen llbuilder else_sstmts);

	let new_else_bb = L.insertion_block llbuilder in

	(* Merge if-elseif-else bbs *)
	let merge_bb = L.append_block context "merge" parent_func in
	L.position_at_end merge_bb llbuilder;

	(* else bb -> else llvalue *)
	let else_bb_val = L.value_of_block new_else_bb in

	(* Go to start bb and add conditional branch to next conditional block *)
	let rec build_cond_brs if_elseif_bbs = match if_elseif_bbs with
		  head :: next :: tail ->
			let head_lexpr, head_start_bb, head_bb, _ = head in
			let _, next_start_bb, next_bb, _ = next in

			L.position_at_end head_start_bb llbuilder;
			ignore(L.build_cond_br head_lexpr head_bb next_start_bb llbuilder);

			build_cond_brs (next :: tail)
		| head :: tail ->
			let head_lexpr, head_start_bb, head_bb, _ = head in

			L.position_at_end head_start_bb llbuilder;
			ignore(L.build_cond_br head_lexpr head_bb else_bb llbuilder)
		| [] -> ()
	in
	build_cond_brs if_elseif_bbs;
	(*L.position_at_end start_bb llbuilder;*)
	(*ignore (L.build_cond_br if_lexpr if_bb else_bb llbuilder);*)

	(* Create merge bb at end of if bb *)
	let rec build_merge_brs if_elseif_bbs = match if_elseif_bbs with
		  head :: tail ->
			let head_lexpr, _, head_bb, new_head_bb = head in

			L.position_at_end new_head_bb llbuilder;
			ignore(L.build_br merge_bb llbuilder);

			build_merge_brs tail
		| [] -> ()
	in
	build_merge_brs if_elseif_bbs;

	(*L.position_at_end new_if_bb llbuilder;*)
	(*ignore (L.build_br merge_bb llbuilder);*)

	(* Create merge bb at end of else bb *)
	L.position_at_end new_else_bb llbuilder;
	ignore (L.build_br merge_bb llbuilder);

	(* Go to end of merge *)
	L.position_at_end merge_bb llbuilder;
	
	else_bb_val

(* Generates a local variable declaration *)
and local_var_gen llbuilder typ id =
	let ltyp = match typ with
	  Obj(classname) -> find_global_class classname
	| _ -> get_llvm_type typ
	in

	let alloc = L.build_alloca ltyp id llbuilder in
	Hash.add local_values id alloc;
	alloc

(* Declare all built-in functions. This should match the functions added in
 * semant.ml
 *)
let construct_library_functions =
	let print_type = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
	in
	let _ = L.declare_function "printf" print_type codegen_module
	in
	() (* return unit *)


let init_params func formals =
	let formal_array = Array.of_list (formals) in
	Array.iteri (fun index value ->
		let name = formal_array.(index) in
		let name = A.string_of_formal name in
		L.set_value_name name value;
		Hash.add local_params name value; ) (L.params func)


let func_stub_gen sfdecl =
	let param_types = List.rev (
		List.fold_left 
			(fun x -> (function A.Formal(t, _) -> get_llvm_type t :: x))
			[] sfdecl.sformals
		) 
	in
	let stype = L.function_type (get_llvm_type sfdecl.stype) (Array.of_list param_types) 
	in
	L.define_function sfdecl.sfname stype codegen_module


let func_body_gen sfdecl =
	Hash.clear local_values;
	Hash.clear local_params;
	let func = func_lookup sfdecl.sfname
	in
	let llbuilder = L.builder_at_end context (L.entry_block func)
	in
	let _ = init_params func sfdecl.sformals
	in
	let _ = sstmt_gen llbuilder (SBlock(sfdecl.sbody))
	in
	(* TODO: Need to generalize this to fit all return types. Right now just 
	 * int. 
	 *)
	L.build_ret (L.const_int i32_t 0) llbuilder



let translate sprogram =
	let _ = construct_library_functions
	(*
	in
	let _ = List.map (fun s -> class_stub_gen s) sprogram.classes
	in
	let _ = List.map (fun s -> class_gen s) sprogram.classes*)
	in

	let _ = List.map (fun f -> func_stub_gen f) sprogram.functions
	in
	let _ = List.map (fun f -> func_body_gen f) sprogram.functions
	in
	codegen_module
