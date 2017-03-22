open Ast

type sexpr =
      SIntLit of int
    | SBoolLit of bool
    | SFloatLit of float
    | SStringLit of string
    | SId of string
    | SBinop of sexpr * op * sexpr * typ
    | SUnop of uop * sexpr * typ
    | SAssign of string * sexpr * typ
    | SCast of typ * sexpr * typ
    | SCall of string * sexpr list * typ
    | SNoexpr

type sstmt = 
      SBlock of sstmt list
    | SExpr of sexpr * typ
    | SReturn of sexpr * typ
    | SIf of sexpr * sstmt * sstmt
    | SElseif of sexpr * sstmt
    | SElseifs of sexpr * sstmt * sstmt list * sstmt
    | SFor of sexpr * sexpr * sexpr * sstmt
    | SWhile of sexpr * sstmt
    | SBreak
    | SContinue
    | SLocalVar of typ * string * sexpr
    | SLocalConst of typ * string * sexpr
    
type sfunc_decl = {
    stype: typ;
    sfname: string;
    sformals: formal_param list;
    (* sbody: sstmt list; *)
    sbody: stmt list;
}

type sclass_body = {
    sfields: field list;
    smethods: sfunc_decl list;
}

type sclass_decl = {
    scname: string;
    scbody: sclass_body;
    (* TODO: add subclasses and interfaces somewhere. *)
}

type sprogram = {
    classes: sclass_decl list;
    functions: sfunc_decl list;
    main: sfunc_decl;
}
