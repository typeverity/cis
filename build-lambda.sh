#!/usr/bin/env fish

docker run -it -w /app -v $PWD:/app --platform linux/x86-64 haskell:9.6.3 \
    bash -c 'cabal update && cabal install --installdir=out'
