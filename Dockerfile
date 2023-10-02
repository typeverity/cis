# syntax=docker/dockerfile:1
FROM ubuntu:23.10 AS build

# Use a login shell to source ~/.profile so we get $PATH set properly so we can find `cabal`.
SHELL [ "/usr/bin/bash", "-l", "-c" ]

ARG GHC_VERSION=9.6.3
ARG CABAL_VERSION=3.10.1.0

RUN apt update && apt install -y build-essential curl zlib1g-dev libffi-dev libffi8 libgmp-dev libgmp10 libncurses-dev libncurses6 libtinfo6

RUN --mount=type=cache,target=/root/.cabal \
    curl --proto '=https' --tlsv1.2 -sSf https://get-ghcup.haskell.org | \
    BOOTSTRAP_HASKELL_NONINTERACTIVE=1 \
    BOOTSTRAP_HASKELL_GHC_VERSION=${GHC_VERSION} \
    BOOTSTRAP_HASKELL_CABAL_VERSION=${CABAL_VERSION} \
    BOOTSTRAP_HASKELL_INSTALL_NO_STACK=1 \
    sh

RUN echo '. ~/.ghcup/env' >> ~/.profile

WORKDIR /build/cis
COPY . /build/cis

RUN --mount=type=cache,target=dist-newstyle --mount=type=cache,target=/root/.cabal \
    cabal configure

RUN --mount=type=cache,target=dist-newstyle --mount=type=cache,target=/root/.cabal \
    cabal build --only-dependencies --only-download

RUN --mount=type=cache,target=dist-newstyle --mount=type=cache,target=/root/.cabal \
    cabal build --only-dependencies

RUN --mount=type=cache,target=dist-newstyle --mount=type=cache,target=/root/.cabal \
    cabal install cisserver --overwrite-policy=always && \
    mkdir /asset/ && \
    cp ~/.cabal/bin/cisserver /asset/

FROM ubuntu:23.10

RUN apt update && apt install -y libffi8 libgmp10 libncurses6 libtinfo6

COPY --from=build /asset/cisserver /asset/

CMD ["bash", "-c", "/asset/cisserver"]
