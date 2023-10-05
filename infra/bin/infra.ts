#!/usr/bin/env node
import "source-map-support/register";
import * as cdk from "aws-cdk-lib";
import { InfraStack } from "../lib/infra-stack";
import { DnsStack } from "../lib/dns-stack";

const app = new cdk.App();

const dnsStack = new DnsStack(app, "DnsStack", {
  env: {
    region: "us-east-1",
  },
  domainNames: ["glenda.loveistheplan.net"],
});

new InfraStack(app, "InfraStack", {
  certificate: dnsStack.certificate,
  domainNames: dnsStack.domainNames,
});
