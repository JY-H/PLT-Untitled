(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)
open Llvm
open Sast

module L = Llvm
module A = Ast
module Hash = Hashtbl
(* Add semantic checker *)
(* module Sem = Semant *)

module StringMap = Map.Make(String)
let global_classes:(string, L.lltype) Hash.t = Hash.create 50
let global_funcs:(string, L.llvalue) Hash.t = Hash.create 50
let global_vars:(string, L.llvalue) Hash.t = Hash.create 50
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
	(* Add tuple/list types *)
	| A.Obj(name) -> L.pointer_type(find_global_class name)
	| _ -> raise (Failure ("Type not yet supported."))

(* Find a global declaration (class/function) if it exists. *)
and find_global_func name = 
	try Hash.find global_funcs name
	with | Not_found -> raise(Failure ("Invalid function name."))
and find_global_class name =
	try Hash.find global_classes name
	with | Not_found -> raise(Failure ("Invalid class name."))   

let construct_library_functions =
    let print_type = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in
    let _ = L.declare_function "print" print_type codegen_module
    in
    () (* return unit *)

let init_params func formals =
	let formal_array = Array.of_list (formals) in
	Array.iteri (fun index value ->
		let name = formal_array.(index) in
		let name = A.string_of_formal name in
			L.set_value_name name value;
			Hash.add local_params name value; ) (L.params func)

let func_stub_gen fdecl =
	let param_types = List.rev (List.fold_left
	(fun x -> (function A.Formal(t, _) -> get_llvm_type t :: x)) []
			fdecl.sformals) in

let func_type = L.function_type (get_llvm_type fdecl.stype)
	(Array.of_list param_types) in
	L.define_function fdecl.sfname func_type codegen_module

let func_body_gen fdecl =
	Hash.clear local_values;
	Hash.clear local_params;
	let func = find_global_func fdecl.sfname in
	let llbuilder = L.builder_at_end context (L.entry_block func) in
	let _ = init_params func fdecl.sformals in ()

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
    (*in
    let _ = main_gen sprogram.main sprogram.classes
    *)
    in
    codegen_module
