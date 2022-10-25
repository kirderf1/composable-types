#!/bin/bash

set -e

default="ghc"

while getopts "c:" opt
do
   case "$opt" in
      c ) compiler="$OPTARG" ;;
      ? ) compiler=$default ;; 
   esac
done

if [ -z "$compiler" ]
then
   compiler=$default
fi

mkdir -p example/output
cabal -v0 v2-run program example example/output --with-compiler=$compiler
$compiler -ilib -iexample/output -o example/output/example example/output/Main.hs
example/output/example
