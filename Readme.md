Prototype Haskell Frontend/LLVM Backend Compiler
================================================

A primitive prototype language using Haskell as the frontend and LLVM as the backend.

Nearly all the code here is from: http://www.stephendiehl.com/llvm/

Current status:
* Frontend and backend are basically hooked together,  languages can now be built from this codebase
* REPL barely useable, symbol replacement is not available, relies on a single main method
* Compilation works better, make scripts are setup to make it easy to write test programs

Plan:
* [x] Build framework able to do both native compilation and REPL based execution
* [x] if/then/else
* [ ] boolean logic
* [ ] basic pattern matching, basic types (algebraic + primitives)
* [ ] List types
* [ ] IO
* [ ] Basic management
* [ ] multiple files, modularity

To build
--------

```
cabal build
```

To compile some code
--------------------

```
cd scratchpad
```

Create a .proto file, e.g.:

hello.proto
```
extern print(arg);

def main() print(1+1);
```

build and run it:
```
make hello
./hello
```

The Makefile will build any .proto files added to the scratchpad directory.

To run the REPL
--------------

Start the REPL:
```
./repl
```

Paste any code you want to run, e.g.:

```
def main() 9*3+2;
```

This will print out the generated LLVM as well as execute it and print the evaluated result.

To run the test suite
---------------------

```
cd tests
make test
```

