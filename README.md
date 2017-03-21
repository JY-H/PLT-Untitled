## DECAF - General Purpose Object Oriented Language
### TODO
* Compile a list of necessary additions to the semantic checker.
* Start on code generation.

### Issues


### How to generate executable from a test program
```
make
./decaf.native < [testfile-name] > [testfile-name].ll
lli [testfile-name].ll
```
* Alternatively, you can call `llc` to get a `.s` file, and then call the gcc
  assembler for an executable.

