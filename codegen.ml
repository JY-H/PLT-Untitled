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
	| _ -> raise (Failure ("Type not yet supported."))

and find_global_class name =
	try Hash.find global_classes name
	with | Not_found -> raise(Failure ("Invalid class name."))	 

(* Truthfully wtf am I doing I'm a monkey *)
let rec id_gen llbuilder id typ is_deref =
	if is_deref then
		try
			Hash.find local_params id
		with Not_found ->
		try
			let new_val = Hash.find local_values id in
			L.build_load new_val id llbuilder
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
	  None -> raise (Failure(" function " ^ fname ^ " does not exit."))
	| Some func -> func 

and string_gen llbuilder s =
	L.build_global_stringptr s "tmp" llbuilder

(*
 * Additionally, this isn't the full case coverage, so we need to add that as
 * well.
 *)
and sstmt_gen llbuilder = function
	  SBlock st -> List.hd(List.map (sstmt_gen llbuilder) st)
	| SExpr(sexpr,t) -> sexpr_gen llbuilder sexpr
	| SLocalVar(typ, id, sexpr) -> local_var_gen llbuilder typ id (* TODO: add
	logic to do assignment, probably create a separate assignment function *)
	| _ -> raise (Failure ("Unknown statement reached."))

and sexpr_gen llbuilder = function
	  SIntLit(i) -> L.const_int i32_t i
	| SBoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
	| SFloatLit(f) -> L.const_float f_t f
	| SStringLit(s) -> string_gen llbuilder s
	| SAssign(sexpr1, sexpr2, typ) -> assign_gen llbuilder sexpr1 sexpr2 typ
	| SCall("print", sexpr_list, typ) -> call_gen llbuilder "printf" sexpr_list
		A.Void
	| SNoexpr -> L.const_int i32_t 0
	| _ -> raise (Failure ("Expression type not recognized.")) 


(* Assignment instruction generation *)
and assign_gen llbuilder sexpr1 sexpr2 typ =
	let rhs_typ = get_type_from_sexpr sexpr2 in

	let lhs, is_obj_access = match sexpr1 with
		  SId(id, typ) -> id_gen llbuilder id typ false, false
		(* TODO: add functionality  for objects, tuples, etc. *)
		(*| SFieldAccess(id, field, typ)) -> *)
		| _ -> raise(Failure("Unable to assign."))
	in

	let rhs = match sexpr2 with
		  SId(id, typ) -> (match typ with
			  Obj(classname) -> id_gen llbuilder id typ false
			| _ -> id_gen llbuilder id typ true)
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

	let rhs = match typ, rhs_typ with
		  Char, Char -> L.build_uitofp rhs i8_t "tmp" llbuilder
		| Int, Int -> L.build_uitofp rhs i32_t "tmp" llbuilder
		| _ -> rhs
	in

	ignore(L.build_store rhs lhs llbuilder);
	rhs


(* Same with call_gen. Need to add full case coverage. *)
and call_gen llbuilder func_name sexpr_list stype =
	match func_name with
	  "printf" -> print_gen llbuilder sexpr_list
	| _ -> raise (Failure ("CALL_GEN -- Not yet supported, go write it yourself."))


(* Helper method to generate printf function for strings. *)
and print_gen llbuilder sexpr_list =
	let params = List.map (fun expr -> sexpr_gen llbuilder expr) sexpr_list
	in
	L.build_call (func_lookup "printf") 
		(Array.of_list ((sexpr_gen llbuilder (SStringLit("%s")))::params))
		"printf" llbuilder

(* TODO: write*)
(* Generates a local variable declaration *)
and local_var_gen llbuilder typ id =
	let noop, ltyp = match typ with
	  Obj(classname) -> (L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0)
	  "noop" llbuilder), find_global_class classname
	| _ -> (L.build_add (L.const_int i32_t 0) (L.const_int i32_t 0) "noop"
		llbuilder), get_llvm_type typ
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
