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

cabal install --enable-tests --enable-benchmarks --only-dependencies deps/* . ${MODE} > install-plan
sed -i -e '1,/^Resolving /d' install-plan
cat install-plan


# Check if the install plan matches the cached cabal snapshot
# -----------------------------------------------------------

if diff -u install-plan $HOME/.cabal/install-plan; then
    echo "Using cabal cache"
else
    echo "Rebuilding cabal cache"
    rm -rf $HOME/.ghc
    rm -rf $HOME/.cabal/bin
    rm -rf $HOME/.cabal/lib
    rm -rf $HOME/.cabal/share
    mv install-plan $HOME/install-plan

    cabal install --enable-tests --enable-benchmarks --only-dependencies deps/* . ${MODE}
    cabal install --enable-tests --enable-benchmarks deps/*
fi

