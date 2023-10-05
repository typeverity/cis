import * as cdk from "aws-cdk-lib";
import {
  ARecord,
  AaaaRecord,
  IHostedZone,
  RecordTarget,
} from "aws-cdk-lib/aws-route53";
import { Construct } from "constructs";

export interface RecordsStackProps extends cdk.StackProps {
  hostedZone: IHostedZone;
  records: Record<string, RecordTarget>;
}

export class RecordsStack extends cdk.Stack {
  records: { a: ARecord; aaaa: AaaaRecord };

  constructor(scope: Construct, id: string, props: RecordsStackProps) {
    super(scope, id, props);

    for (const [name, target] of Object.entries(props.records)) {
      new ARecord(this, `ARecord-${name}`, {
        zone: props.hostedZone,
        target,
      });
      new AaaaRecord(this, `AaaaRecord-${name}`, {
        zone: props.hostedZone,
        target,
      });
    }
  }
}
