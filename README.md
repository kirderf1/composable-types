# composable-types

A transpiler/transformation for an experimental language extension to Haskell, meant to make it easier to implement ASTs (and similar data types) in a composable way.
The node variants of the AST can be split into pieces, which can then be composed to form the type of the AST.

Currently does not work with GHC 9+ due to the dependency on the package compdata (which is used by our testsuite).

This transpiler was written as part of a thesis, which is not yet finished.
