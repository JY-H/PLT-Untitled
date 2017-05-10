open Ast

type sexpr =
      SIntLit of int
    | SBoolLit of bool
    | SFloatLit of float
    | SCharLit of char
    | SStringLit of string
    | SId of string * typ
    | SNull
    | SBinop of sexpr * op * sexpr * typ
    | SUnop of uop * sexpr * typ
    | SAssign of sexpr * sexpr * typ
    | SCast of typ * sexpr
	| SArrayCreate of sexpr list * typ
	| SSeqAccess of sexpr * sexpr * typ
    | SFieldAccess of sexpr * sexpr * typ
    | SCall of string * sexpr list * typ
    | SMethodCall of sexpr * string * sexpr list * typ
    | SObjCreate of typ * sexpr list
    | SNoexpr

type sstmt = 
      SBlock of sstmt list
    | SExpr of sexpr * typ
    | SReturn of sexpr * typ
    | SIf of sexpr * sstmt * sstmt list * sstmt
    | SElseif of sexpr * sstmt
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
    sbody: sstmt list;
}

type sclass_body = {
    sfields: field list;
    smethods: sfunc_decl list;
}

type sclass_decl = {
    scname: string;
    scbody: sclass_body;
}

type sprogram = {
    classes: sclass_decl list;
    functions: sfunc_decl list;
    reserved: sfunc_decl list;
}
