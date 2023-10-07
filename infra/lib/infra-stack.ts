import * as cdk from "aws-cdk-lib";
import { EndpointType, LambdaRestApi } from "aws-cdk-lib/aws-apigateway";
import { ICertificate } from "aws-cdk-lib/aws-certificatemanager";
import { Distribution, IDistribution } from "aws-cdk-lib/aws-cloudfront";
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
  IHostedZone,
  RecordTarget,
} from "aws-cdk-lib/aws-route53";
import { CloudFrontTarget } from "aws-cdk-lib/aws-route53-targets";
import { Construct } from "constructs";

export interface InfraStackProps extends cdk.StackProps {
  domainNames: string[];
  certificate: ICertificate;
  hostedZone: IHostedZone;
}

export class InfraStack extends cdk.Stack {
  cf: IDistribution;

  constructor(scope: Construct, id: string, props: InfraStackProps) {
    super(scope, id, props);

    const lambda = new Function(this, "Lambda", {
      architecture: Architecture.ARM_64,
      runtime: Runtime.PROVIDED_AL2,
      handler: "api-gateway", // this has to be the same as the name given in ../server/Main.hs to `runWaiAsLambda`.
      // We want to see execution traces of our Lambda so we can drill down into the code when something goes wrong. There are two ways of instrumenting Lambdas: Xray and OpenTelemetry (or otel for short). Xray is a vendor (AWS) specific protocol. Otel is a standard that anyone can implement. Using otel means that, in addition to CloudWatch, we can send our tracing data to Honeycomb or Datadog instead. AWS graciously provides a ready-made Lambda layer for otel instrumentation called ADOT (AWS Distro for OpenTelemetry) and we can configure it directly in CDK/CloudFormation.
      adotInstrumentation: {
        layerVersion: AdotLayerVersion.fromGenericLayerVersion(
          // Using LATEST here might be a bad idea, since any deployment is no longer idempotent.
          AdotLambdaLayerGenericVersion.LATEST
        ),
        // Our Lambda only understands AWS API Gateway Proxy requests, so we need to tell that to the ADOT instrumentation, which hooks into the request processing pipeline and so needs to know what kind of requests to expect.
        execWrapper: AdotLambdaExecWrapper.PROXY_HANDLER,
      },
      environment: {
        OTEL_SERVICE_NAME: "cisserver",
        OTEL_PROPAGATORS: "tracecontext,baggage,awsxray", // Adding awsxray here doesn't actually work, cf. https://github.com/iand675/hs-opentelemetry/issues/59. We work around this by explicitly adding the propagator in our Main.hs file. TODO: check if we actually need this propagator at all, or does the Lambda instrumentation layer handle this for us?
      },
      // This is what we deploy. We assume that the Lambda binary has already been built. Everything in ../out will be zipped and deployed as our Lambda. The directory should have a binary named `bootstrap`, which Lambda will call for incoming requests. It can contain other files as well.
      code: Code.fromAsset("../out"),
    });

    const apiGW = new LambdaRestApi(this, "APIGateway", {
      handler: lambda,
      deployOptions: { tracingEnabled: true },
      // Since we're using CloudFront in front of our API Gateway, it makes architecturally more sense to use regional endpoints here, instead of edge endpoints, since CF already caches at the edge. If we didn't use CloudFront we could use edge Lambdas and enable their own cache. That cache has a hourly cost though, unlike CloudFront, which is purely billed on data transfer and request count.
      endpointTypes: [EndpointType.REGIONAL],
    });

    this.cf = new Distribution(this, "CloudFront", {
      defaultBehavior: {
        origin: new RestApiOrigin(apiGW),
      },
      certificate: props.certificate,
      domainNames: props.domainNames,
    });

    const cfTarget = RecordTarget.fromAlias(new CloudFrontTarget(this.cf));

    for (const domain of props.domainNames) {
      new ARecord(this, `ARecord`, {
        target: cfTarget,
        zone: props.hostedZone,
        recordName: domain,
      });
      new AaaaRecord(this, `AaaaRecord${domain}`, {
        target: cfTarget,
        zone: props.hostedZone,
        recordName: domain,
      });
    }
  }
}
