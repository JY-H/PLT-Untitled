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

and func_lookup fname = match (L.lookup_function fname codegen_module) with
      None -> raise (Failure(" function " ^ fname ^ " does not exit."))
    | Some func -> func 

and string_gen llbuilder s =
    L.build_global_stringptr s "tmp" llbuilder

(* For both sstmt_gen and sexpr_gen:
    * Everything here needs to be converted to sast format. Eg, it should be
    * Sast.SBlock st -> ...
    * However since right now we are using the func_body without
    * modification/semantic checking, we are doing this. I suggest working on
    * the semant.ml first to get expr->sexpr, stmt->sstmt translation working.
    *
 * Additionally, this isn't the full case coverage, so we need to add that as
 * well.
 *)
and sstmt_gen llbuilder = function
      A.Block st -> List.hd(List.map (sstmt_gen llbuilder) st)
    | A.Expr sexpr -> sexpr_gen llbuilder sexpr
    | _ -> raise (Failure ("Unknown statement reached."))

and sexpr_gen llbuilder = function
      A.IntLit(i) -> L.const_int i32_t i
    | A.BoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
    | A.FloatLit(f) -> L.const_float f_t f
    | A.StringLit(s) -> string_gen llbuilder s
    | A.FuncCall("print", sexpr_list) -> call_gen llbuilder "printf" sexpr_list
    A.Void
    | A.Noexpr -> L.const_int i32_t 0
    | _ -> raise (Failure ("Expression type not recognized.")) 


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
        (Array.of_list ((sexpr_gen llbuilder (A.StringLit("%s")))::params))
        "printf" llbuilder
    

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
    (* TODO: Corresponds to the above comments. Block needs to be changed to
     * SBlock once support is added in semant.ml
     *)
    let _ = sstmt_gen llbuilder (Block(sfdecl.sbody))
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
