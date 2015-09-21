#!/bin/bash

# echo commands, exit immediately on error
set -xe

# Fetch git dependencies
# ----------------------

function fetch()
{
    git clone $2 deps/$1
    pushd deps/$1
    git reset --hard $3
    popd
}

fetch accelerate ${URL_ACCELERATE} ${SHA_ACCELERATE}


# Create an install plan
# ----------------------

cabal install --enable-tests --enable-benchmarks --only-dependencies --verbose --dry-run deps/* . ${MODE} > install-plan
sed -i -e '1,/^Resolving /d' install-plan
cat install-plan


# Check if the install plan matches the cached cabal snapshot
# -----------------------------------------------------------

if diff -u install-plan $HOME/.cabal-snapshot/install-plan; then
    echo "Using cabal cache"
    rm -rf $HOME/.ghc
    cp -a $HOME/.cabal-snapshot/ghc $HOME/.ghc
    cp -a $HOME/.cabal-snapshot/bin $HOME/.cabal/
    cp -a $HOME/.cabal-snapshot/lib $HOME/.cabal/
    cp -a $HOME/.cabal-snapshot/share $HOME/.cabal/
else
    echo "Rebuilding cabal cache"
    rm -rf $HOME/.cabal-snapshot

    cabal install --enable-tests --enable-benchmarks --only-dependencies deps/* . ${MODE}
    cabal install --enable-tests --enable-benchmarks deps/*

    # make a snapshot of the cabal directory
    mkdir $HOME/.cabal-snapshot
    cp -a $HOME/.ghc $HOME/.cabal-snapshot/ghc
    cp -a $HOME/.cabal/bin $HOME/.cabal-snapshot/
    cp -a $HOME/.cabal/lib $HOME/.cabal-snapshot/
    cp -a $HOME/.cabal/share $HOME/.cabal-snapshot/
    cp -a install-plan $HOME/.cabal-snapshot/
fi

