(* Semantic checking for the DECAF compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

type class_map = {
    class_decl: Ast.class_decl;
    functions: Ast.func_decl StringMap.t;
    fields: Ast.field StringMap.t; 
}
let global_func_map = StringMap.empty


(* Helper function to get all built-in functions. *)
let get_reserved_funcs = 
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

(* Helper function to get fully qualified names so that we can distinguish
 * between global functions and class functions of the same name
 *)
let get_fully_qualified_name class_name fdecl = match fdecl.fname with
      "main" -> "main"
    | _ ->  class_name ^ "." ^ fdecl.fname 


(* Put all global function declarations into a map *)
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
                fields = List.fold_left map_fields StringMap.empty
                    cdecl.cbody.fields;
                functions = List.fold_left map_functions StringMap.empty
                    cdecl.cbody.methods;
                class_decl = cdecl;
            } map
        )
    in List.fold_left map_class StringMap.empty cdecls

(*
let parse_stmt stmt = function
      Ast.Block blk -> 

let get_sbody_from_body stmt_list = 
    let rec get_sstmt = function
          hd::tl -> 
              let sstmt = parse_stmt hd
              in
              sstmt::(get_sstmt tl)
        | [] -> []
    in
    let sstmt_list = get_sstmt stmt_list
    in
    sstmt_list
*)

let get_sfdecl_from_fdecl fdecl =
    let func_sbody = (* get_sbody_from_body fdecl.body *) fdecl.body
    in
    {
        stype = fdecl.return_typ;
        sfname = fdecl.fname;
        sformals = fdecl.formals;
        sbody = func_sbody;
    }

(* Helper method to extract sfdecls from fdecls within classes. *)
let get_class_fdecls class_maps = 
    (* First use StringMap.fold to extract class decls from class_maps *)
    let class_fdecls = StringMap.fold (
        fun cname c l -> 
            (* Then extract fdecls within each cdecl and convert to sfdecl. *)
            let get_class_fnames result = StringMap.fold (
                fun fname f l -> (get_sfdecl_from_fdecl f)::l
            ) c.functions result 
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
        let reserved_funcs = get_reserved_funcs in
        let reserved_map = List.fold_left (
            fun map f -> StringMap.add f.sfname f map
        ) StringMap.empty reserved_funcs
        in
        let global_func_map = get_global_func_map globals.fdecls reserved_map in
        let class_maps = get_class_maps globals.cdecls reserved_map in
        let sast = get_sast class_maps global_func_map reserved_funcs globals.cdecls globals.fdecls in
        sast
    (* TODO: A lot of shit. But check is the main function so we need to add top
     * level logic here. *)
