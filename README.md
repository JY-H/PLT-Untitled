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
Every pull request should come with a test case to prove that the newly implemented functionality fully works.

#### Apr.1st: get everything you can do in main (aka everything you can do in C) done
* priority:
* functions/returns
* objects

#### TESTING TODOs:
* Remember to add additional testing scripts if you implement a new functionality.

### Issues

### LRM Changelist
* Allow global functions outside of class declarations (rather than just `main` as was originally proposed).
* Remove `interface` from the list of functionalities.
