## DECAF - General Purpose Object Oriented Language

### Usages
```
eval `opam config env`
make
./decaf.native [-flag] < [testfile.dcf]
```
* Use `-a` to test ast generation and semant checker.
* Use `-l` to generate LLVM IR.
* Alternatively, you can save the LLVM IR to a `.ll` file and generate assembly from the IR code.
  ```
  eval `opam config env`
  make
  ./decaf.native -l < [testfile.dcf] | tail -n+3 > [testfile.ll]
  llc [testfile.ll]
  ```
  * You can then use the gcc assembly to generate an executable.
    ```
    gcc [testfile.s]
    ```

### TODO
~`hello_world` Immediate TODOs~
##### `semant.ml` TODOs:
* Currently we do not fully walk and semantically verify the AST. Some parts are merely copied directly from the AST. (For example, see the `get_sfdecl_from_fdecl` function). Thus, we need to:
  * Write `expr->sexpr` translation.
  * Write `stmt->sstmt` translation (partially written).
* This is necessary in order to further work on `codegen.ml`.
##### `codegen.ml` TODOs:
* Once we do another pass at `semant.ml`, we can flush out the different sections in `codegen.ml`. For details, see comments above some of the codegen functions.
##### TESTING TODOs:
* Remember to add additional testing scripts if you implement a new functionality.

### Issues

### LRM Changelist
* Allow global functions outside of class declarations (rather than just `main` as was originally proposed).
* Remove `interface` from the list of functionalities.
