# cis

This is a toy Haskell AWS Lambda function behind CloudFront and API Gateway. 

The [infra](./infra) repository contains all the CDK code for the infrastructure. The entry point is [bin/infra.ts](./infra/bin/infra.ts).

[.github/workflows/haskell.yml](.github/workflows/haskell.yml) contains the continuous deployment pipeline. Dragons abound here.

[server](./server) contains the actual Lambda code. It just serves a few hardcoded values. The interesting bits are that it emits OpenTelemetry to the Lambda otel layer (specified in the infrastructure code) and that it's just a regular [wai](https://hackage.haskell.org/package/wai) app in the end.

[ghc.Dockerfile](ghc.Dockerfile) lets us build a static executable that just works in the Lambda AL2 environment, thanks to https://github.com/benz0li/ghc-musl.

## running locally

1. Run [run-jaeger.sh](./run-jaeger.sh) to start a tracing backend and open a tracing UI. (If it doesn't open automatically, go to http://localhost:16686.)
2. ??? Have to figure out how to run this lambda thing locally. SAM might help.
3. Sorry