#!/bin/bash

set -e

mkdir -p build
cabal -v0 v2-run program Common.hs > build/Output.hs
ghc -ilib -o build/output build/Output.hs
build/output
