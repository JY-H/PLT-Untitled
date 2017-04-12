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

### NOTES
Every pull request should come with a test case to prove that the newly implemented functionality fully works.

### TODO:
* objects
	- need to refactor how we deal with primitives and data types to handle addition of classes
	- check_field_access still needs to be fleshed out

#### TESTING TODOs:
* Remember to add additional testing scripts if you implement a new functionality.

### Issues

### LRM Changelist
* Allow global functions outside of class declarations (rather than just `main` as was originally proposed).
* Remove `interface` from the list of functionalities.
