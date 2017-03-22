(* Top-level of the DECAF compiler: scan & parse the input,
 * check the resulting AST, generate LLVM IR, and dump the module. *)
module L = Llvm

type action = Ast | LLVM_IR | Compile

let _ = let action = if Array.length Sys.argv > 1 then
    List.assoc Sys.argv.(1) [ ("-a", Ast);
                              ("-l", LLVM_IR);
                              ("-c", Compile) ]
    else Ast in
    let lexbuf = Lexing.from_channel stdin in
    let ast = Parser.program Scanner.token lexbuf in
    (* let sast = Semant.check ast in *)
    match action with
      Ast -> print_string (Ast.string_of_program ast)
    | LLVM_IR -> print_string (L.string_of_llmodule (Codegen.translate (*sast*)
    ast))
    | Compile -> let m = Codegen.translate (*sast*) ast in
      Llvm_analysis.assert_valid_module m;
      print_string (Llvm.string_of_llmodule m)
