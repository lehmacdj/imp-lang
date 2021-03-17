# IMP
An implementation of a very basic imperative language.

The language has support for basic arithmetic and boolean operations, integer
variables, if statements, and while loops, while there is no capability for
custom IO such as printing or reading input, it is possible to perform somewhat
interesting computations through use of the environment which is printed when a
program terminates. For example you can write to a variable called result at the
end of program execution, and consider that value.

You will need [Haskell stack](https://docs.haskellstack.org/en/stable/README/)
(recommended since it works reliably across Linux/macOS/Windows) or a working
cabal installation to use this project.

This project builds an executable `impi` which launches an interpreter for
programs in the language. To run it do:
```
stack build && stack exec impi
```
This will get you a REPL that can evaluate Arithmetic and Boolean expressions
(AExp/BExp respectively in source), in addition to commands (including sequences
of commands separated by semi-colon). The environment is preserved between
individual commands you issue.

The `impi` executable is also capable of running complete programs from files
with the `.imp` extension. You can do this by running a commandline like
```
stack build && stack exec impi -- examples/fibonacci.imp
```
Various examples to try running are present in the examples folder.

See also:
https://github.com/lehmacdj/talk-how-to-implement-a-programming-langugae-in-10-minute
For a short tutorial that goes through the development of a language a lot like
this one.
