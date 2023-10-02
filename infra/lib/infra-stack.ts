import * as cdk from "aws-cdk-lib";
import { LambdaRestApi, RestApi } from "aws-cdk-lib/aws-apigateway";
import { Platform } from "aws-cdk-lib/aws-ecr-assets";
import {
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
      code: DockerImageCode.fromImageAsset("..", {
        platform: Platform.LINUX_ARM64,
      }),
    });

    const apiGW = new LambdaRestApi(this, "APIGateway", {
      handler: dockerLambda,
    });
  }
}
