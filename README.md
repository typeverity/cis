# cis

This is a toy Haskell AWS Lambda function behind CloudFront and API Gateway. 

The [infra](./infra) repository contains all the CDK code for the infrastructure.

[.github/workflows/haskell.yml](.github/workflows/haskell.yml) contains the continuous deployment pipeline.

[server](./server) contains the actual Lambda code. It just serves a few hardcoded values. The interesting bits are that it emits OpenTelemetry to the Lambda otel layer (specified in the infrastructure code) and that it's just a regular [wai](https://hackage.haskell.org/package/wai) app in the end.
