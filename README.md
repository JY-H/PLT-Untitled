## DECAF - General Purpose Object Oriented Language
### TODO
* Finish testing AST generation using MENHIR.
* Make automated testing so we can pipe all test cases into menhir without having to try them all manually when we change the parser. Make a file with all the test cases and pipe to menhir, compare with file containing expecte output (ACCEPT/REJECT)
* Compile a list of necessary additions to the semantic checker.
* Start on code generation.

### Issues
* Allow program to contain functions outside of classes? Current implementation is that everything has to be wrapped in a class declaration. 
