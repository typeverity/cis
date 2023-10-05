import * as cdk from "aws-cdk-lib";
import { EndpointType, LambdaRestApi } from "aws-cdk-lib/aws-apigateway";
import { ICertificate } from "aws-cdk-lib/aws-certificatemanager";
import { Distribution } from "aws-cdk-lib/aws-cloudfront";
import { RestApiOrigin } from "aws-cdk-lib/aws-cloudfront-origins";
import {
  AdotLambdaExecWrapper,
  AdotLambdaLayerGenericVersion,
  AdotLayerVersion,
  Architecture,
  Code,
  Function,
  Runtime,
} from "aws-cdk-lib/aws-lambda";
import {
  ARecord,
  AaaaRecord,
  HostedZone,
  RecordTarget,
} from "aws-cdk-lib/aws-route53";
import { CloudFrontTarget } from "aws-cdk-lib/aws-route53-targets";
import { Construct } from "constructs";

export interface InfraStackProps extends cdk.StackProps {
  domainNames: string[];
  certificate: ICertificate;
}

export class InfraStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props: InfraStackProps) {
    super(scope, id, props);

    const lambda = new Function(this, "Lambda", {
      architecture: Architecture.ARM_64,
      runtime: Runtime.PROVIDED_AL2,
      handler: "api-gateway",
      adotInstrumentation: {
        layerVersion: AdotLayerVersion.fromGenericLayerVersion(
          AdotLambdaLayerGenericVersion.LATEST
        ),
        execWrapper: AdotLambdaExecWrapper.PROXY_HANDLER,
      },
      environment: {
        OTEL_SERVICE_NAME: "cisserver",
        OTEL_PROPAGATORS: "tracecontext,baggage,awsxray",
      },
      code: Code.fromAsset("../out"),
    });

    const apiGW = new LambdaRestApi(this, "APIGateway", {
      handler: lambda,
      deployOptions: { tracingEnabled: true },
      endpointTypes: [EndpointType.REGIONAL],
    });

    const cf = new Distribution(this, "CloudFront", {
      defaultBehavior: {
        origin: new RestApiOrigin(apiGW),
      },
      certificate: props.certificate,
      domainNames: props.domainNames,
    });

    const hostedZone = HostedZone.fromLookup(this, "HostedZone", {
      domainName: props.domainNames[0],
    });

    new ARecord(this, "ARecord", {
      target: RecordTarget.fromAlias(new CloudFrontTarget(cf)),
      zone: hostedZone,
    });

    new AaaaRecord(this, "AaaaRecord", {
      target: RecordTarget.fromAlias(new CloudFrontTarget(cf)),
      zone: hostedZone,
    });

    new cdk.CfnOutput(this, "CloudFrontDistributionDomainName", {
      value: cf.distributionDomainName,
    });

    new cdk.CfnOutput(this, "CloudFrontDomainName", {
      value: cf.domainName,
    });
  }
}
