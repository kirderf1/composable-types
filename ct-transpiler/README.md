# Program Layout

Code for the transpiler itself is located in /src/. /testsuite/ contains a testsuite program alongside its test files. /app/ contains a simple program for testing the transpiler on some file(s) provided as arguments. /lib/ contains a library file that is used by code outputted by the transform. /example/ contains some example modules written in our syntax.

# Testsuite

Can be run with `cabal v2-run testsuite`.
The testsuite consists of transform tests that check the transformation code output against an exact expected output, and compile tests that check that ghc can compile the transformation code output.

For ghc to handle references to compdata, a version of ghc must be used that compdata supports, and which has compdata installed. Since ct-transpiler depends on compdata, that library should be installed by cabal before the testsuite is run, compdata will be present if the testsuite is using the same version of ghc as cabal. The version of ghc used by the testsuite can be specified with the `--compiler ghc-a.b.c` option. To run the testsuite with ghc-8.10, simply run:

> `cabal v2-run --with-compiler ghc-8.10 testsuite -- --compiler ghc-8.10`

Note: At the moment, the testsuite can only find its tests if run from this folder (/ct-transpiler/).
