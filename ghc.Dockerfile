FROM glcr.b-data.ch/ghc/ghc-musl:9.6.3

ARG uid=1001

RUN adduser glenda -D -u ${uid}

USER glenda
