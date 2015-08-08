#!/bin/sh
set -e

echo "Did you update the .cabal version and ChangeLog? (y/n)"
read answer
if [[ "$answer" != "y" ]]; then
  echo "Opening Changelog.md and halive.cabal..."
  subl ChangeLog.md
  subl halive.cabal
  exit 1
fi

echo "Building with 7.10..."
cabal configure -w /Applications/ghc-7.10.2.app/Contents/bin/ghc
cabal build
echo "Building with 7.8..."
cabal configure -w /Applications/ghc-7.8.4.app/Contents/bin/ghc
cabal build
cabal sdist

echo "Restoring 7.10 config..."
cabal configure -w /Applications/ghc-7.10.2.app/Contents/bin/ghc
