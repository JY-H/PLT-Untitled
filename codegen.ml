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

let global_jank_counter = 0

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

let rec id_gen llbuilder id is_deref =
	let x = match is_deref with
		true -> "true"
		| false -> "false"
	in print_string x;

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
and sstmt_gen llbuilder loop_stack = function
	(* NOTE: this requires a function body to be non-empty, is this ok? *)
	  SBlock(stmts) -> List.hd(List.map (sstmt_gen llbuilder loop_stack) stmts)
	| SExpr(sexpr, _) -> sexpr_gen llbuilder sexpr
	| SIf(if_sexpr, if_stmts, elseifs, else_sstmts) ->
		if_gen llbuilder loop_stack if_sexpr if_stmts elseifs else_sstmts
	| SFor(sexpr1, sexpr2, sexpr3, sstmts) ->
		for_gen llbuilder loop_stack sexpr1 sexpr2 sexpr3 sstmts
	| SWhile(sexpr, sstmts) -> while_gen llbuilder loop_stack sexpr sstmts
	| SBreak -> break_gen llbuilder loop_stack
	| SContinue -> continue_gen llbuilder loop_stack
	| SLocalVar(typ, id, sexpr) -> ignore(local_var_gen llbuilder typ id);
		assign_gen llbuilder (SId(id, typ)) sexpr typ
	| _ -> raise(Failure("Unknown statement reached."))

and sexpr_gen llbuilder = function
	  SIntLit(i) -> L.const_int i32_t i
	| SBoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
	| SFloatLit(f) -> L.const_float f_t f
	| SStringLit(s) -> string_gen llbuilder s
	| SId(id, typ) -> print_int 3; id_gen llbuilder id true
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
		  SId(id, typ) -> print_int 2; id_gen llbuilder id false, false
		(* TODO: add functionality  for objects, tuples, etc. *)
		(*| SFieldAccess(id, field, typ)) -> *)
		| _ -> raise(Failure("Unable to assign."))
	in

	let rhs = match sexpr2 with
		  SId(id, typ) -> (match typ with
			  Obj(classname) -> print_int 5; id_gen llbuilder id false
			| _ -> print_int 1; id_gen llbuilder id true)
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

and if_gen llbuilder loop_stack if_sexpr if_sstmts elseifs else_sstmts =
	(* if expr *)
	(*let if_lexpr = sexpr_gen llbuilder if_sexpr in*)

	(* Initial bb *)
	let start_bb = L.insertion_block llbuilder in
	let parent_func = L.block_parent start_bb in

	let if_bb = L.append_block context "if" parent_func in
	(* if bb *)
	let if_body_bb = L.append_block context "if_body" parent_func in

	(* Create if bb statements *)
	L.position_at_end if_body_bb llbuilder;
	ignore(sstmt_gen llbuilder loop_stack if_sstmts);

	let new_if_body_bb = L.insertion_block llbuilder in

	(* elseif bbs *)
	let rec make_elseif_bbs elseifs = match elseifs with
		  [] -> []
		| head :: tail ->
			let selseif = match head with
				  SElseif(sexpr, sstmt) -> (sexpr, sstmt)
				| _ -> raise(Failure("Unexpected non-elseif in elseif list"))
			in
			let elseif_sexpr, elseif_sstmts = selseif in

			(* elseif bb *)
			let bb = L.insertion_block llbuilder in
			let elseif_bb = L.append_block context "elseif" parent_func in
			let elseif_body_bb = L.append_block context "elseif_body"
				parent_func in

			(* Create elseif bb statements *)
			L.position_at_end elseif_body_bb llbuilder;
			ignore(sstmt_gen llbuilder loop_stack elseif_sstmts);

			let new_elseif_bb = L.insertion_block llbuilder in
			(elseif_sexpr, elseif_bb, elseif_body_bb, new_elseif_bb) :: 
				make_elseif_bbs tail
	in
	let elseif_bbs = make_elseif_bbs elseifs in
	let if_elseif_bbs = (if_sexpr, if_bb, if_body_bb, new_if_body_bb) ::
		elseif_bbs in
	
	(* else bb *)
	let else_body_bb = L.append_block context "else_body" parent_func in

	(* Create else bb statements *)
	L.position_at_end else_body_bb llbuilder;
	ignore(sstmt_gen llbuilder loop_stack else_sstmts);

	let new_else_body_bb = L.insertion_block llbuilder in

	(* Merge if-elseif-else bbs *)
	let merge_bb = L.append_block context "merge" parent_func in
	L.position_at_end merge_bb llbuilder;

	(* else bb -> else llvalue *)
	let else_bb_val = L.value_of_block new_else_body_bb in

	(* Initial bb -> if bb *)
	L.position_at_end start_bb llbuilder;
	ignore(L.build_br if_bb llbuilder);

	(* Go to start bb and add conditional branch to next conditional block *)
	let rec build_cond_brs if_elseif_bbs = match if_elseif_bbs with
		  head :: next :: tail ->
			let head_sexpr, head_bb, head_body_bb, _ = head in
			let _, next_bb, next_body_bb, _ = next in

			(* Build expr for cond br, then build cond br *)
			L.position_at_end head_bb llbuilder;
			let head_lexpr = sexpr_gen llbuilder head_sexpr in
			ignore(L.build_cond_br head_lexpr head_body_bb next_bb llbuilder);

			build_cond_brs (next :: tail)
		| head :: tail ->
			let head_sexpr, head_bb, head_body_bb, _ = head in

			L.position_at_end head_bb llbuilder;
			let head_lexpr = sexpr_gen llbuilder head_sexpr in
			ignore(L.build_cond_br head_lexpr head_body_bb else_body_bb llbuilder)
		| [] -> ()
	in
	build_cond_brs if_elseif_bbs;

	(* Create merge bb at end of if bb *)
	let rec build_merge_brs if_elseif_bbs = match if_elseif_bbs with
		  head :: tail ->
			let _, _, _, new_head_bb = head in

			L.position_at_end new_head_bb llbuilder;
			ignore(L.build_br merge_bb llbuilder);

			build_merge_brs tail
		| [] -> ()
	in
	build_merge_brs if_elseif_bbs;

	(* Create merge bb at end of else bb *)
	L.position_at_end new_else_body_bb llbuilder;
	ignore(L.build_br merge_bb llbuilder);

	(* Go to end of merge *)
	L.position_at_end merge_bb llbuilder;
	
	else_bb_val

and for_gen llbuilder loop_stack sexpr1 sexpr2 sexpr3 sstmts =
	(*let old_val = !is_loop in
	is_loop := true;*)
	
	let parent_func = L.block_parent (L.insertion_block llbuilder) in

	(* Build initialization expr *)
	ignore(sexpr_gen llbuilder sexpr1);

	(* bb's for body, step, condition, and exit *)
	let body_bb = L.append_block context "loop_body" parent_func in
	let step_bb = L.append_block context "loop_step" parent_func in
	let cond_bb = L.append_block context "loop_cond" parent_func in
	let exit_bb = L.append_block context "loop_exit" parent_func in

	(*if not old_val then
		continue_block := step_bb;
		break_bb = exit_bb;
	in*)
	loop_stack := (step_bb, exit_bb) :: !loop_stack;

	(* Init block -> cond *)
	ignore(L.build_br cond_bb llbuilder);

	(* Reorder blocks: bb, step, cond, exit*)
	let bb = L.insertion_block llbuilder in
	L.move_block_after bb step_bb;
	L.move_block_after step_bb cond_bb;
	L.move_block_after cond_bb exit_bb;
	ignore(L.build_br step_bb llbuilder);
	(* At exit bb, jump to step bb *)

	(* Build step *)
	L.position_at_end step_bb llbuilder;
	ignore(sexpr_gen llbuilder sexpr3);
	ignore (L.build_br cond_bb llbuilder);

	(* Build conditional branch to exit bb *)
	L.position_at_end cond_bb llbuilder;
	let cond_lexpr = sexpr_gen llbuilder sexpr2 in
	ignore(L.build_cond_br cond_lexpr body_bb exit_bb llbuilder);

	(* Build body bb stmts *)
	L.position_at_end body_bb llbuilder;
	ignore(sstmt_gen llbuilder loop_stack sstmts);
	ignore(L.build_br step_bb llbuilder);

	(*is_loop := old_val;*)
	let remove_loop stack = match stack with
		  [] -> []
		| head :: tail -> tail
	in
	loop_stack := remove_loop !loop_stack;

	(*Continue building from loop exit *)
	L.position_at_end exit_bb llbuilder;

	L.const_null i32_t

and while_gen llbuilder loop_stack sexpr sstmts =
	for_gen llbuilder loop_stack (SIntLit(0)) sexpr (SIntLit(0)) sstmts

(* Branch to nearest loop exit *)
and break_gen llbuilder loop_stack =
	match !loop_stack with
		  head :: tail ->
			L.build_br (snd head) llbuilder
		| [] -> raise(Failure("Break found in non-loop"))

(* Branch to nearest loop step *)
and continue_gen llbuilder loop_stack =
	match !loop_stack with
		  head :: tail ->
			L.build_br (fst head) llbuilder
		| [] -> raise(Failure("Continue found in non-loop"))

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
	(* Stack of control flow blocks *)
	let loop_stack = ref [] in
	let _ = sstmt_gen llbuilder loop_stack (SBlock(sfdecl.sbody))
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
