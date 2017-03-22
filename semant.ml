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

(* Helper function to get fully qualified names so that we can distinguish
 * between global functions and class functions of the same name
 *)
let get_fully_qualified_name class_name fdecl = match fdecl.fname with
      "main" -> "main"
    | _ ->  class_name ^ "." ^ fdecl.fname 


(* Put all global function declarations into a map *)
let get_global_func_map fdecls = 
    let map_global_funcs map fdecl = 
        if (StringMap.mem fdecl.fname global_func_map) then
            raise (Failure(" duplicate global function: " ^ fdecl.fname))
        else
            StringMap.add fdecl.fname fdecl map
    in
    List.fold_left map_global_funcs StringMap.empty fdecls


(* Pull all class internals into a map, including declaration, functions, and
 * fields.
 *)
let get_class_maps cdecls =
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


(* TODO: Need to write stmt -> sstmt conversion. But this should be enough for
 * just hello_world
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


let get_sast class_maps global_func_maps cdecls fdecls  =
    let find_main f = match f.sfname with 
        "main" -> true
        | _ -> false
    in
    let get_main functions = 
        let main_decls = List.find_all find_main functions
        in
        if (List.length main_decls < 1) then
            raise (Failure("Main not defined."))
        else if (List.length main_decls > 1) then
            raise (Failure("More than 1 main function defined."))
        else
            List.hd main_decls
    in 
    let get_sfdecls l f =
        let sfdecl = get_sfdecl_from_fdecl f in sfdecl::l
    in
    let sfdecls = List.fold_left get_sfdecls [] fdecls
    in
    {
        classes = []; 
        functions = sfdecls;
        main = get_main sfdecls;
    }

let check program = match program with
    Program(globals) ->  
        let global_func_map = get_global_func_map globals.fdecls in
        let class_maps = get_class_maps globals.cdecls in
        let sast = get_sast class_maps global_func_map globals.cdecls globals.fdecls in
        sast
    (* TODO: A lot of shit. But check is the main function so we need to add top
     * level logic here. *)
