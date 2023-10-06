#!/usr/bin/env node
import "source-map-support/register";
import * as cdk from "aws-cdk-lib";
import { InfraStack } from "../lib/infra-stack";
import { DnsStack } from "../lib/dns-stack";

const app = new cdk.App();

const appDomain = "glenda.loveistheplan.net";

// We use CloudFront as our CDN, and it requires that our HTTPS certificate is in the us-east-1 region, so let's create a separate DNS stack there.
const dnsStack = new DnsStack(app, "DnsStack", {
  env: {
    region: "us-east-1",
    account: process.env.CDK_DEFAULT_ACCOUNT,
  },
  crossRegionReferences: true,
  domainNames: [appDomain],
});

// This is our (badly named) application stack that contains our Lambda, API Gateway and CloudFront distribution. It also creates the DNS records to point to our CloudFront distribution (see `appDomain` above). The end result is that the end-user can navigate to https://glenda.loveistheplan.net/item to call our Lambda (or, more likely, to get a cached response from CloudFront).
new InfraStack(app, "InfraStack", {
  env: { account: process.env.CDK_DEFAULT_ACCOUNT, region: "eu-north-1" },
  crossRegionReferences: true,
  certificate: dnsStack.certificate,
  domainNames: dnsStack.domainNames,
  hostedZone: dnsStack.hostedZone,
});
