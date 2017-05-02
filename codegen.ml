(* Code generation: translate takes a semantically checked AST and
produces LLVM IR

LLVM tutorial: Make sure to read the OCaml version of the tutorial

http://llvm.org/docs/tutorial/index.html

Detailed documentation on the OCaml LLVM library:

http://llvm.moe/
http://llvm.moe/ocaml/

*)

(*open Llvm*)
open Sast
open Semant
(*open Str*)

module L = Llvm
module A = Ast 
module S = Sast
module Se = Semant

module Hash = Hashtbl

module StringMap = Map.Make(String)
let global_classes:(string, L.lltype) Hash.t = Hash.create 50
let local_params:(string, L.llvalue) Hash.t = Hash.create 50
let local_values:(string, L.llvalue) Hash.t = Hash.create 50
let class_self:(string, L.llvalue) Hash.t = Hash.create 50
let class_fields:(string, int) Hash.t = Hash.create 50

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
        | A.Null_t -> i32_t
	(* TODO: Add tuple/list types *)
        | A.ClassTyp(name) -> L.pointer_type(find_global_class name)
	| _ -> raise(Failure("Type not yet supported."))

and find_global_class name =
	try Hash.find global_classes name
	with Not_found -> raise(Failure("Invalid class name " ^ name))

let rec id_gen llbuilder id is_deref =
	if is_deref then
		if Hash.mem local_values id then
			let _val = Hash.find local_values id in
			L.build_load _val id llbuilder
		else if Hash.mem local_params id then
			Hash.find local_params id
		else
			raise(Failure("Unknown variable " ^ id))
	else
		if Hash.mem local_values id then
			Hash.find local_values id
		else if Hash.mem local_params id then
			Hash.find local_params id
		else
			raise(Failure("Unknown variable " ^ id))

and func_lookup fname = match (L.lookup_function fname codegen_module) with
		  None -> raise(Failure(" function " ^ fname ^ " does not exist."))
		| Some func -> func

and string_gen llbuilder s =
	L.build_global_stringptr s "tmp" llbuilder

and sstmt_gen llbuilder loop_stack = function
	(* NOTE: this requires a function body to be non-empty, is this ok? *)
	  SBlock(stmts) -> List.hd(List.map (sstmt_gen llbuilder loop_stack) stmts)
	| SExpr(sexpr, _) -> sexpr_gen llbuilder sexpr
		| SReturn(sexpr, t) -> ret_gen llbuilder sexpr t
	| SIf(if_sexpr, if_stmts, elseifs, else_sstmts) ->
		if_gen llbuilder loop_stack if_sexpr if_stmts elseifs else_sstmts
	| SFor(sexpr1, sexpr2, sexpr3, sstmts) ->
		for_gen llbuilder loop_stack sexpr1 sexpr2 sexpr3 sstmts
	| SWhile(sexpr, sstmts) -> while_gen llbuilder loop_stack sexpr sstmts
	| SBreak -> break_gen llbuilder loop_stack
	| SContinue -> continue_gen llbuilder loop_stack
	| SLocalVar(typ, id, sexpr) -> local_var_gen llbuilder typ id sexpr
	| SLocalConst(typ, id, sexpr) -> local_var_gen llbuilder typ id sexpr
	| _ -> raise(Failure("Unknown statement reached."))

and sexpr_gen llbuilder = function
	  SIntLit(i) -> L.const_int i32_t i
	| SBoolLit(b) -> if b then L.const_int i1_t 1 else L.const_int i1_t 0
	| SCharLit(c) -> L.const_int i8_t (Char.code c)
	| SFloatLit(f) -> L.const_float f_t f
	| SStringLit(s) -> string_gen llbuilder s
		| SId(id, _) -> id_gen llbuilder id true
		| SNull -> L.const_null i32_t
	| SBinop(sexpr1, op, sexpr2, typ) ->
		binop_gen llbuilder sexpr1 op sexpr2 typ
	| SUnop(op, sexpr, typ) ->
		unop_gen llbuilder op sexpr typ
	| SAssign(sexpr1, sexpr2, typ) -> assign_gen llbuilder sexpr1 sexpr2 typ
	| SCast(to_typ, sexpr) -> cast_gen llbuilder to_typ sexpr
        | SFieldAccess(c, rhs, typ) -> field_access_gen llbuilder c rhs typ true
	| SCall(fname, sexpr_list, stype) -> call_gen llbuilder fname sexpr_list stype
        | SMethodCall(sexpr, fname, sexpr_list, stype) -> method_call_gen llbuilder sexpr fname sexpr_list stype (* difference is insert self as the first argument *)
        | SObjCreate(typ, sexprl) -> obj_create_gen llbuilder typ sexprl
		| SNoexpr -> L.const_int i32_t 0
	| _ -> raise(Failure("Expression type not recognized.")) 

and binop_gen llbuilder sexpr1 op sexpr2 typ =
	let lexpr1 = sexpr_gen llbuilder sexpr1 in
	let lexpr2 = sexpr_gen llbuilder sexpr2 in

	(* KILL_ME: kill this if unused by end of project *)
	let typ1 = get_type_from_sexpr sexpr1 in
	let typ2 = get_type_from_sexpr sexpr2 in

	let int_ops expr1 binop expr2 = match binop with
		  A.Add -> L.build_add expr1 expr2 "int_addtmp" llbuilder
		| A.Sub -> L.build_sub expr1 expr2 "int_subtmp" llbuilder
		| A.Mult -> L.build_mul expr1 expr2 "int_multop" llbuilder
		| A.Div -> L.build_sdiv expr1 expr2 "int_divop" llbuilder
		| A.Mod -> L.build_srem expr1 expr2 "int_modop" llbuilder
		| A.Veq -> L.build_icmp L.Icmp.Eq expr1 expr2 "int_eqtmp" llbuilder
		| A.Vneq -> L.build_icmp L.Icmp.Ne expr1 expr2 "int_neqtmp" llbuilder
		| A.Less -> L.build_icmp L.Icmp.Slt expr1 expr2 "int_lesstmp" llbuilder
		| A.Leq -> L.build_icmp L.Icmp.Sle expr1 expr2 "int_leqtmp" llbuilder
		| A.Greater -> L.build_icmp L.Icmp.Sgt expr1 expr2 "int_greatertmp" llbuilder
		| A.Geq -> L.build_icmp L.Icmp.Sge expr1 expr2 "int_geqtmp" llbuilder
				| A.And -> L.build_and expr1 expr2 "int_andtmp" llbuilder
				(*I think several others are missing, like or*)
		| _ -> raise(Failure("Unsupported operator for integers"))
	in
	
	let float_ops expr1 binop expr2 = match binop with
		  A.Add -> L.build_fadd expr1 expr2 "flt_addtmp" llbuilder
		| A.Sub -> L.build_fsub expr1 expr2 "flt_subtmp" llbuilder
		| A.Mult -> L.build_fmul expr1 expr2 "flt_multop" llbuilder
		| A.Div -> L.build_fdiv expr1 expr2 "flt_divop" llbuilder
		| A.Mod -> L.build_frem expr1 expr2 "flt_modop" llbuilder
		| A.Veq -> L.build_fcmp L.Fcmp.Oeq expr1 expr2 "flt_eqtmp" llbuilder
		| A.Vneq -> L.build_fcmp L.Fcmp.One expr1 expr2 "flt_neqtmp" llbuilder
		| A.Less -> L.build_fcmp L.Fcmp.Olt expr1 expr2 "flt_lesstmp" llbuilder
		| A.Leq -> L.build_fcmp L.Fcmp.Ole expr1 expr2 "flt_leqtmp" llbuilder
		| A.Greater -> L.build_fcmp L.Fcmp.Ogt expr1 expr2 "flt_greatertmp" llbuilder
		| A.Geq -> L.build_fcmp L.Fcmp.Oge expr1 expr2 "flt_geqtmp" llbuilder
		| _ -> raise(Failure("Unsupported operator for floats"))
	in

	(* TODO: do something for req/rneq here *)
	
	match typ with
		  A.Int -> int_ops lexpr1 op lexpr2
		| A.Float -> float_ops lexpr1 op lexpr2
		| A.Bool -> match typ1, typ2 with
			A.Int, A.Int | A.Bool, A.Bool -> int_ops lexpr1 op lexpr2
			| A.Float, A.Float -> float_ops lexpr1 op lexpr2
			| _, _ -> raise(Failure("Cannot perform operations on types "
				^ A.string_of_typ typ1 ^ " and " ^ A.string_of_typ typ2))
		| _ -> raise(Failure("Unrecognized data type in binop"))

and unop_gen llbuilder unop sexpr typ =
	let unop_lval = sexpr_gen llbuilder sexpr in
	
	let build_unop op unop_typ lval = match op, unop_typ with
		  A.Neg, A.Int -> L.build_neg lval "neg_int_tmp" llbuilder
		| A.Neg, A.Float -> L.build_fneg lval "neg_flt_tmp" llbuilder
		| A.Not, A.Bool -> L.build_not lval "not_bool_tmp" llbuilder
		| _ -> raise(Failure("Unsupported unop for " ^ A.string_of_uop op ^
			" and type " ^ A.string_of_typ typ))
	in

	match typ with
		  A.Int | A.Float | A.Bool -> build_unop unop typ unop_lval
		| _ -> raise(Failure("Invalid type for unop: " ^ A.string_of_typ typ))

and cast_gen llbuilder to_typ sexpr =
	(* TODO: check this is legit *)
	let lexpr = sexpr_gen llbuilder sexpr in
	let from_typ = get_type_from_sexpr sexpr in
	match from_typ with
		  A.Bool -> (
			let zero = L.const_int i1_t 0 in
			match to_typ with
			  A.Bool -> lexpr
			| A.Int -> L.build_zext lexpr i32_t "bool_int_cast" llbuilder
			(* NOTE: questionable implementation *)
			| A.Float -> L.build_uitofp lexpr f_t "bool_float_cast" llbuilder
			(* NOTE: non-working implementation unless arrays are used *)
			(*| String ->
				L.build_global_stringptr (A.string_of_lval lexpr)
				"bool_string_cast" llbuilder*)
			| _ -> raise(Failure("Invalid cast from " ^ A.string_of_typ from_typ ^
				" to " ^ A.string_of_typ to_typ))
			)
		| A.Int -> (match to_typ with
			  A.Int -> lexpr
			| A.Bool ->
				let zero = L.const_int i32_t 0 in
				L.build_icmp L.Icmp.Ne lexpr zero "int_bool_cast" llbuilder
			| A.Char -> L.build_trunc lexpr i8_t "int_char_cast" llbuilder
			(* NOTE: questionable implementation *)
			| A.Float -> L.build_sitofp lexpr f_t "int_float_cast" llbuilder
			(* NOTE: non-working implementation unless arrays are used *)
			(*| String
				L.build_global_stringptr (A.string_of_lval lexpr)
				"int_string_cast" llbuilder*)
			| _ -> raise(Failure("Invalid cast from " ^ A.string_of_typ from_typ ^
				" to " ^ A.string_of_typ to_typ))
			)
		| A.Float -> (match to_typ with
			  A.Float -> lexpr
			| A.Bool ->
				let zero = L.const_float f_t 0.0 in
				L.build_fcmp L.Fcmp.One lexpr zero "float_bool_cast" llbuilder
			(* NOTE: questionable implementation *)
			| A.Int -> L.build_fptosi lexpr i32_t "float_int_cast" llbuilder
			(* NOTE: non-working implementation unless arrays are used *)
			(*| A.String ->
				L.build_global_stringptr (A.string_of_lval lexpr)
				"float_string_cast" llbuilder*)
			| _ -> raise(Failure("Invalid cast from " ^ A.string_of_typ from_typ ^
				" to " ^ A.string_of_typ to_typ))
			)
		| A.Char -> (match to_typ with
			  A.Char -> lexpr
			| A.Int -> L.build_zext lexpr i32_t "char_int_cast" llbuilder
			(* TODO: string conversion when strings exist *)
			| _ -> raise(Failure("Invalid cast from " ^ A.string_of_typ from_typ ^
				" to " ^ A.string_of_typ to_typ))
			)
		(* TODO; string conversion when strings exist *)
		| _ -> raise(Failure("Invalid cast from " ^ A.string_of_typ from_typ ^
			" to " ^ A.string_of_typ to_typ))
	(* TODO: cast objects when objects are a thing *)

(* Assignment instruction generation *)
and assign_gen llbuilder sexpr1 sexpr2 typ =
	(* KILL_ME: kill this if unused by end of project *)
	(*let rhs_typ = get_type_from_sexpr sexpr2 in*)

	let lhs, is_obj_access = match sexpr1 with
		  SId(id, _) -> id_gen llbuilder id false, false
		(* TODO: add functionality  for tuples, etc. *)
		| SFieldAccess(id, field, typ) -> field_access_gen llbuilder id field typ false, true
		| _ -> raise(Failure("Unable to assign."))
	in

	let rhs = match sexpr2 with
		  SId(id, typ) -> (match typ with
			  A.ClassTyp(classname) -> id_gen llbuilder id false
			| _ -> id_gen llbuilder id true)
		| SFieldAccess(id, field, typ) -> field_access_gen llbuilder id field typ true
		| _ -> sexpr_gen llbuilder sexpr2
	in

	let rhs = match typ with
		  A.ClassTyp(classname) ->
			if is_obj_access then
				rhs
			else
				L.build_load rhs "tmp" llbuilder
                | A.Null_t -> L.const_null (get_llvm_type typ)
		| _ -> rhs
	in

	ignore(L.build_store rhs lhs llbuilder);
	rhs

and field_access_gen llbuilder id rhs typ isAssign =
    let check_id id =
        match id with
         SId(s, d) -> id_gen llbuilder s false
        (* array *)
        | _ -> raise(Failure("expected access lhs to be an id"))
    in

    let rec check_rhs isLHS par_exp par_typ rhs =
        let class_name = A.string_of_typ par_typ in
        match rhs with
        SId(s, d) ->
            let field_name = (class_name ^ "." ^ s) in
			(*print_int 1;
			print_string (field_name ^ "\n");*)
            let field_index = Hash.find class_fields field_name in
			(*print_int 2;*)
            let _val = L.build_struct_gep par_exp field_index s llbuilder in
            let _val = match d with
                A.ClassTyp(_) -> if isAssign then L.build_load _val s llbuilder else _val
                | _ -> if isAssign then L.build_load _val s llbuilder else _val
            in
            _val
        | SCall(fname, exprl, ftyp) -> call_gen llbuilder fname exprl ftyp
        | SFieldAccess(e1, e2, d) ->
                let e1_typ = Se.get_type_from_sexpr e1 in
                let e1 = check_rhs true par_exp par_typ e1 in
                let e2 = check_rhs true e1 e1_typ e2 in
                e2
        | _ -> raise(Failure("illegal rhs type for access"))
    in
    let id_typ = Se.get_type_from_sexpr id in
    let id = check_id id in
    let rhs = check_rhs true id id_typ rhs in
    rhs

and obj_create_gen llbuilder typ sexprl =
    let f = func_lookup (A.string_of_typ typ) in
    let params = List.map (sexpr_gen llbuilder) sexprl in
    let obj = L.build_call f (Array.of_list params) "tmp" llbuilder in
    obj

and cast_malloc_gen llbuilder sexprl stype =
    let cast_malloc llbuilder lhs curTyp newTyp =
        match newTyp with
            A.ClassTyp(c) -> let obj_llvm_typ = get_llvm_type (A.ClassTyp(c)) in L.build_pointercast lhs obj_llvm_typ "tmp" llbuilder
        | _ as c -> raise(Failure("RIP cannot cast to " ^ A.string_of_typ c))
    in
    let sexpr = List.hd sexprl in
    let old_typ = Se.get_type_from_sexpr sexpr in
    let lhs = match sexpr with
          SId(id, d) -> id_gen llbuilder id false
        | SFieldAccess(e1, e2, d) -> field_access_gen llbuilder e1 e2 d false
        | _ -> sexpr_gen llbuilder sexpr
    in
    cast_malloc llbuilder lhs old_typ stype

and method_call_gen llbuilder obj_expr fname sexprl styp =
    match obj_expr with
    SId(obj_name, obj_typ) ->
        let the_func = func_lookup fname in
        let match_sexpr se = match se with
          SId(id, d) -> let isDeref = match d with
                  A.ClassTyp(_) -> false
                | _ -> true
        in id_gen llbuilder id isDeref
        | se -> sexpr_gen llbuilder se
        in
        let lhs = match_sexpr obj_expr in
        let self_param = L.build_pointercast lhs (get_llvm_type obj_typ) "tmp" llbuilder in
        let params = List.map match_sexpr sexprl in
        match styp with
                A.Void -> L.build_call the_func (Array.of_list (self_param :: params)) "" llbuilder
        | _ -> L.build_call the_func (Array.of_list (self_param :: params)) "tmp" llbuilder
    | _ -> raise(Failure("unsupported chained method call"))

and func_call_gen llbuilder fname sexprl stype =
    let the_func = func_lookup fname in
    let params = List.map (sexpr_gen llbuilder) sexprl in
    match stype with
	A.Void -> L.build_call the_func (Array.of_list params) "" llbuilder
        | _ -> L.build_call the_func (Array.of_list params) "tmp" llbuilder

and call_gen llbuilder fname sexprl stype =
		match fname with
                (* full list of built-in and linked functions just for clarity*)
				 "print" -> print_gen llbuilder sexprl
                                | "malloc" -> func_call_gen llbuilder fname sexprl stype
                                | "cast" -> cast_malloc_gen llbuilder sexprl stype
				| _ -> func_call_gen llbuilder fname sexprl stype

(* Helper method to generate print function for strings. *)
and print_gen llbuilder sexpr_list =
	let params = List.map (fun expr -> sexpr_gen llbuilder expr) sexpr_list
	in
	L.build_call (func_lookup "printf")
		(Array.of_list ((sexpr_gen llbuilder (SStringLit("%s")))::params))
		"print" llbuilder

and ret_gen llbuilder sexpr t =
	match sexpr with
		  SId(name, t) ->
                     (match t with
                     | A.ClassTyp(_) -> L.build_ret (id_gen llbuilder name false) llbuilder
                     | _ -> L.build_ret (id_gen llbuilder name true) llbuilder)
                | SFieldAccess(e1, e2, d) -> L.build_ret (field_access_gen llbuilder e1 e2 d true) llbuilder
                | SNoexpr ->
			L.build_ret_void llbuilder
		| _ ->
			L.build_ret (sexpr_gen llbuilder sexpr) llbuilder

and if_gen llbuilder loop_stack if_sexpr if_sstmts elseifs else_sstmts =
	(* if expr *)
	(* KILL_ME *)
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
			let _, next_bb, _, _ = next in

			(* Build expr for cond br, then build cond br *)
			L.position_at_end head_bb llbuilder;
			let head_lexpr = sexpr_gen llbuilder head_sexpr in
			ignore(L.build_cond_br head_lexpr head_body_bb next_bb llbuilder);

			build_cond_brs (next :: tail)
		| head :: _ ->
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
	let parent_func = L.block_parent (L.insertion_block llbuilder) in

	(* Build initialization expr *)
	ignore(sexpr_gen llbuilder sexpr1);

	(* bb's for body, step, condition, and exit *)
	let body_bb = L.append_block context "loop_body" parent_func in
	let step_bb = L.append_block context "loop_step" parent_func in
	let cond_bb = L.append_block context "loop_cond" parent_func in
	let exit_bb = L.append_block context "loop_exit" parent_func in

	let new_loop_stack = (step_bb, exit_bb) :: loop_stack in

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
	ignore(sstmt_gen llbuilder new_loop_stack sstmts);
	ignore(L.build_br step_bb llbuilder);

	(*Continue building from loop exit *)
	L.position_at_end exit_bb llbuilder;

	L.const_null i32_t

and while_gen llbuilder loop_stack sexpr sstmts =
	for_gen llbuilder loop_stack (SIntLit(0)) sexpr (SIntLit(0)) sstmts

(* Branch to nearest loop exit *)
and break_gen llbuilder loop_stack =
	match loop_stack with
		  head :: _ ->
			L.build_br (snd head) llbuilder
		| [] -> raise(Failure("Break found in non-loop"))

(* Branch to nearest loop step *)
and continue_gen llbuilder loop_stack =
	match loop_stack with
		  head :: _ ->
			L.build_br (fst head) llbuilder
		| [] -> raise(Failure("Continue found in non-loop"))

(* Generates a local variable declaration *)
and local_var_gen llbuilder typ id sexpr =
	let ltyp = match typ with
          A.ClassTyp(classname) -> find_global_class classname
	| _ -> get_llvm_type typ
	in

	let alloc = L.build_alloca ltyp id llbuilder in
	Hash.add local_values id alloc;
		let lhs = SId(id, typ) in
		match sexpr with
				SNoexpr -> alloc
				| _ -> assign_gen llbuilder lhs sexpr typ

(* Declare all built-in functions. This should match the functions added in
 * semant.ml
 *)
let construct_library_functions =
	let print_type = L.var_arg_function_type i32_t [| L.pointer_type i8_t |]
	in
	let _ = L.declare_function "printf" print_type codegen_module
	in
        let malloc_type = L.function_type (str_t) [| i32_t |] in
        let _ = L.declare_function "malloc" malloc_type codegen_module
        in
	()

let init_params func formals =
	let formal_array = Array.of_list (formals) in
	Array.iteri (fun index value ->
		let name = formal_array.(index) in
				match name with A.Formal(_, n) ->
		L.set_value_name n value;
		Hash.add local_params n value; ) (L.params func)

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
	let func = func_lookup sfdecl.sfname in
	let llbuilder = L.builder_at_end context (L.entry_block func) in
	let _ = init_params func sfdecl.sformals in
	(* Stack of loop blocks *)
	let loop_stack = [] in
	let _ = sstmt_gen llbuilder loop_stack (SBlock(sfdecl.sbody)) in
		if sfdecl.stype = A.Void then
			ignore(L.build_ret_void llbuilder);
		ignore(L.build_unreachable llbuilder)

let class_gen s =
        let typ = L.named_struct_type context s.scname in
        Hash.add global_classes s.scname typ;

	let typ = Hash.find global_classes s.scname in
	let typ_lst = List.map (function
		A.ObjVar(t, _, _) | A.ObjConst(t, _, _) ->
			get_llvm_type t) s.scbody.sfields in
	let name_lst = List.map (function
		A.ObjVar(_, n, _) | A.ObjConst(_, n, _) ->
			n) s.scbody.sfields in
	(* adding i32_t and key *)
	let typ_array = (Array.of_list typ_lst) in
		List.iteri (fun i name ->
			let full_name = s.scname ^ "." ^ name in
			Hash.add class_fields full_name i;
		) name_lst;
        L.struct_set_body typ typ_array true;

	let _ = List.map (fun f -> func_stub_gen f) s.scbody.smethods in
	let res = List.map (fun f -> func_body_gen f) s.scbody.smethods in
        res


(*TBA: probably needed when inheritance happens
let vtbl_gen scdecls =
            *)

let translate sprogram =
	let _ = construct_library_functions in
        let _ = if (List.length sprogram.classes > 0) then List.map (fun s -> class_gen s) sprogram.classes else [] in
	let _ = List.map (fun f -> func_stub_gen f) sprogram.functions in
	let _ = List.map (fun f -> func_body_gen f) sprogram.functions in
        (*        let _ = vtbl_gen sprogram.classes in*)
	codegen_module
