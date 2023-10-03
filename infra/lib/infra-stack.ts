import * as cdk from "aws-cdk-lib";
import { LambdaRestApi } from "aws-cdk-lib/aws-apigateway";
import { Distribution } from "aws-cdk-lib/aws-cloudfront";
import { RestApiOrigin } from "aws-cdk-lib/aws-cloudfront-origins";
import { Platform } from "aws-cdk-lib/aws-ecr-assets";
import {
  AdotLambdaExecWrapper,
  AdotLambdaLayerGenericVersion,
  AdotLayerVersion,
  Architecture,
  DockerImageCode,
  DockerImageFunction,
} from "aws-cdk-lib/aws-lambda";
import { Construct } from "constructs";

export class InfraStack extends cdk.Stack {
  constructor(scope: Construct, id: string, props?: cdk.StackProps) {
    super(scope, id, props);

    const dockerLambda = new DockerImageFunction(this, "DockerLambda", {
      architecture: Architecture.ARM_64,
      adotInstrumentation: {
        layerVersion: AdotLayerVersion.fromGenericLayerVersion(
          AdotLambdaLayerGenericVersion.LATEST
        ),
        execWrapper: AdotLambdaExecWrapper.PROXY_HANDLER,
      },
      code: DockerImageCode.fromImageAsset("..", {
        platform: Platform.LINUX_ARM64,
      }),
    });

    const apiGW = new LambdaRestApi(this, "APIGateway", {
      handler: dockerLambda,
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
