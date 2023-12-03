#!/usr/bin/env node
import "source-map-support/register";
import * as cdk from "aws-cdk-lib";
import { InfraStack } from "../lib/infra-stack";
import { DnsStack } from "../lib/dns-stack";

const app = new cdk.App();

const tags = {
  Application: "cisserver",
  Environment: "development",
};

const appDomain = "glenda.loveistheplan.net";

// We use CloudFront as our CDN, and it requires that our HTTPS certificate is in the us-east-1 region, so let's create a separate DNS stack there. This stack is badly named as it only creates the certificate for the given `domainNames`.
const dnsStack = new DnsStack(app, "DnsStack", {
  env: {
    // Create the certificate in us-east-1 so CloudFront can use it.
    region: "us-east-1",
    // We could specify a concrete account here, but there is no real reason to do that. We do need to specify _some_ account, as otherwise CDK gets confused with cross-account references. `CDK_DEFAULT_ACCOUNT` is the account we're deploying to at any given time, so it doesn't tie this stack to any specific account.
    account: process.env.CDK_DEFAULT_ACCOUNT,
  },
  // This makes it magically possible for our InfraStack below to refer to values from this stack.
  crossRegionReferences: true,
  // These are the domain names we want to create a certificate for. We only need one here, but certificates support multiple domain names, so this property is more general than we actually need.
  domainNames: [appDomain],
  tags,
});

// This is our (badly named) application stack that contains our Lambda, API Gateway and CloudFront distribution. It also creates the DNS records to point to our CloudFront distribution (see `appDomain` above). The end result is that the end-user can navigate to https://glenda.loveistheplan.net/item to call our Lambda (or, more likely, to get a cached response from CloudFront).
new InfraStack(app, "InfraStack", {
  env: {
    // eu-north-1 is our main region.
    region: "eu-north-1",
    // As above, we need to specify _some_ account here, otherwise CDK gets confused. This is us saying "it's fine, don't worry" to CDK.
    account: process.env.CDK_DEFAULT_ACCOUNT,
  },
  // We need to enable cross region references in this stack as well to be able to refer to values in DnsStack.
  crossRegionReferences: true,
  // CloudFront will use this certificate.
  certificate: dnsStack.certificate,
  // CloudFront will use these domain names, and we will create A and AAAA DNS records for these that point to CloudFront.
  domainNames: dnsStack.domainNames,
  // This is the hosted zone where the above records will be created.
  hostedZone: dnsStack.hostedZone,
  tags,
});
