(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

module L = Llvm
module A = Ast
(* Add semantic checke *)
(* module Sem = Semant *)

module StringMap = Map.Make(String)
let program_global_decls:(string, L.lltype) Hash.t = Hash.create 50

let context = L.global_context()
let codegen_module = L.create_module context "DECAF Codegen"
let builder = L.builder context

let i1_t = L.i1_type context;;
let i8_t = L.i8_type context;;
let i32_t = L.i32_type econtext;;
let f_t = L.double_type context;;
let str_t = L.pointer_type i8_t;;
let void_t = L.void_type context;;

let get_llvm_type = function
      A.Int -> i32_t
    | A.Float -> f_t
    | A.Bool -> i1_t
    | A.Char -> i8_t
    | A.Void -> void_t
    | A.String -> str_t
    (* Add tuple/list types *)
    | A.Obj(name) -> L.pointer_type(find_global_decl name)
    | _ -> raise (Failure ("Type not yet supported."))

(* Find a global declaration (class/function) if it exists. *)
and find_global_decl name = 
   try Hash.find program_global_decls name
   with | Not_found -> raise(Failure ("Invalid class/function name."))
   

let construct_library_functions =
    let print_type = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
    in
    let _ = L.declare_function "print" print_type codegen_module
    in
    () (* return unit *)

let translate sprogram =
    let _ = construct_library_functions
    (*
    in
    let _ = List.map (fun s -> class_stub_gen s) sprogram.classes
    in
    let _ = List.map (fun s -> class_gen s) sprogram.classes
    *)
    in

    let _ = List.map (fun f -> func_stub_gen f) sprogram.functions
    in
    let _ = List.map (fun f -> func_body_gen f) sprogram.functions
    in
    let _ = main_gen sprogram.main sprogram.classes
    in
    codegen_module
