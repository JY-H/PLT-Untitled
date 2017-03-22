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
* Compile a list of necessary additions to the semantic checker.
* Start on code generation.

### Issues

