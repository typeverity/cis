#!/usr/bin/env node
import "source-map-support/register";
import * as cdk from "aws-cdk-lib";
import { InfraStack } from "../lib/infra-stack";
import { DnsStack } from "../lib/dns-stack";

const app = new cdk.App();

const appDomain = "glenda.loveistheplan.net";

const dnsStack = new DnsStack(app, "DnsStack", {
  env: {
    region: "us-east-1",
    account: process.env.CDK_DEFAULT_ACCOUNT,
  },
  crossRegionReferences: true,
  domainNames: [appDomain],
});

new InfraStack(app, "InfraStack", {
  env: { account: process.env.CDK_DEFAULT_ACCOUNT, region: "eu-north-1" },
  crossRegionReferences: true,
  certificate: dnsStack.certificate,
  domainNames: dnsStack.domainNames,
  hostedZone: dnsStack.hostedZone,
});
