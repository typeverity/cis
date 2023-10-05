import * as cdk from "aws-cdk-lib";
import {
  Certificate,
  CertificateValidation,
  ICertificate,
} from "aws-cdk-lib/aws-certificatemanager";
import { HostedZone } from "aws-cdk-lib/aws-route53";
import { Construct } from "constructs";

export interface DnsStackProps extends cdk.StackProps {
  domainNames: string[];
}

export class DnsStack extends cdk.Stack {
  certificate: ICertificate;
  domainNames: string[];

  constructor(scope: Construct, id: string, props: DnsStackProps) {
    super(scope, id, props);

    if (props.domainNames.length === 0) {
      throw new Error("No domain names provided");
    }

    this.certificate = new Certificate(this, "Certificate", {
      domainName: props.domainNames[0],
      subjectAlternativeNames: props.domainNames.slice(1),
      validation: CertificateValidation.fromDns(
        HostedZone.fromLookup(this, "HostedZone", {
          domainName: "loveistheplan.net",
        })
      ),
    });

    this.domainNames = props.domainNames;
  }
}
