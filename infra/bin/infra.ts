#!/usr/bin/env node
import "source-map-support/register";
import * as cdk from "aws-cdk-lib";
import { InfraStack } from "../lib/infra-stack";
import { DnsStack } from "../lib/dns-stack";

const app = new cdk.App();

const appDomain = "glenda.loveistheplan.net";

// We use CloudFront as our CDN, and it requires that our HTTPS certificate is in the us-east-1 region, so let's create a separate DNS stack there. This stack is badly named as it only creates the certificate for the given `domainNames`.
const dnsStack = new DnsStack(app, "DnsStack", {
  env: {
    // Create the certificate in us-east-1 so CloudFront can use it.
    region: "us-east-1",
    // We could specify a concrete account here, but there is no real reason to do that. We do need to specify _some_ account, as otherwise CDK gets confused with cross-account references. `CDK_DEFAULT_ACCOUNT` is the account we're deploying to at any given time, so it doesn't tie this stack to any specific account.
    account: process.env.CDK_DEFAULT_ACCOUNT,
  },
  crossRegionReferences: true,
  domainNames: [appDomain],
});

// This is our (badly named) application stack that contains our Lambda, API Gateway and CloudFront distribution. It also creates the DNS records to point to our CloudFront distribution (see `appDomain` above). The end result is that the end-user can navigate to https://glenda.loveistheplan.net/item to call our Lambda (or, more likely, to get a cached response from CloudFront).
new InfraStack(app, "InfraStack", {
  env: {
    // eu-north-1 is our main region.
    region: "eu-north-1",
    // As above, we need to specify _some_ account here, otherwise CDK gets confused. This is us saying "it's fine, don't worry" to CDK.
    account: process.env.CDK_DEFAULT_ACCOUNT,
  },
  crossRegionReferences: true,
  certificate: dnsStack.certificate,
  domainNames: dnsStack.domainNames,
  hostedZone: dnsStack.hostedZone,
});
