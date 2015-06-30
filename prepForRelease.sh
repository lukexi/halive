#!/bin/sh
set -e

echo "Building with 7.10..."
cabal configure -w /Applications/ghc-7.10.1.app/Contents/bin/ghc
cabal build
echo "Building with 7.8..."
cabal configure -w /Applications/ghc-7.8.4.app/Contents/bin/ghc
cabal build
cabal sdist

echo "Restoring 7.10 config..."
cabal configure -w /Applications/ghc-7.10.1.app/Contents/bin/ghc