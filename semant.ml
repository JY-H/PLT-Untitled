(* Semantic checking for the DECAF compiler *)

open Ast
open Sast

module StringMap = Map.Make(String)

type class_map = {
    class_decl: Ast.class_decl;
    functions: Ast.func_decl StringMap.t;
    fields: Ast.field StringMap.t; 
}

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

        (* Map all functions. *)
        let map_functions map fdecl =
            if (StringMap.mem fdecl.fname map) then
                raise (Failure(" duplicate function: " ^ fdecl.fname))
            else
                StringMap.add fdecl.fname fdecl map
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

let get_sast class_maps cdecls =
    let find_main = function
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
    (* TODO: Add something to extract functions and replace [] with it. Line
     * 795. *)
    in get_main []

let check (classes, funcs)  =
    let class_maps = get_class_maps classes 
    in
    let sast = get_sast class_maps funcs
    in
    sast
    (* TODO: A lot of shit. But check is the main function so we need to add top
     * level logic here. *)
