# composable-types

A transpiler for an experimental language extension to Haskell, meant to make it easier to implement ASTs in a composable way.
The node variants of the AST can be split into pieces, which can then be composed to form the type of the AST.

Currently does not work with GHC 9+ due to the dependency on the package compdata.
