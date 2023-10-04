import * as cdk from "aws-cdk-lib";
import { LambdaRestApi } from "aws-cdk-lib/aws-apigateway";
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
import { Construct } from "constructs";

export class InfraStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
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
      code: Code.fromAsset("../out"),
    });

    const apiGW = new LambdaRestApi(this, "APIGateway", {
      handler: lambda,
    });

    const cf = new Distribution(this, "CloudFront", {
      defaultBehavior: {
        origin: new RestApiOrigin(apiGW),
      },
    });

    new cdk.CfnOutput(this, "CloudFrontDistributionDomainName", {
      value: cf.distributionDomainName,
    });

    new cdk.CfnOutput(this, "CloudFrontDomainName", {
      value: cf.domainName,
    });
  }
}
