#!/usr/bin/env node
import "source-map-support/register";
import * as cdk from "aws-cdk-lib";
import { InfraStack } from "../lib/infra-stack";
import { DnsStack } from "../lib/dns-stack";
import { RecordsStack } from "../lib/records-stack";
import { RecordTarget } from "aws-cdk-lib/aws-route53";
import { CloudFrontTarget } from "aws-cdk-lib/aws-route53-targets";

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

const infraStack = new InfraStack(app, "InfraStack", {
  env: { account: process.env.CDK_DEFAULT_ACCOUNT, region: "eu-north-1" },
  crossRegionReferences: true,
  certificate: dnsStack.certificate,
  domainNames: dnsStack.domainNames,
});

new RecordsStack(app, "RecordStack", {
  env: { account: process.env.CDK_DEFAULT_ACCOUNT, region: "us-east-1" },
  crossRegionReferences: true,
  hostedZone: dnsStack.hostedZone,
  records: Object.fromEntries(
    dnsStack.domainNames.map((name) => [
      name,
      RecordTarget.fromAlias(new CloudFrontTarget(infraStack.cf)),
    ])
  ),
});
