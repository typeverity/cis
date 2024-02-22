import * as cdk from "aws-cdk-lib";
import { EndpointType, LambdaRestApi } from "aws-cdk-lib/aws-apigateway";
import { ICertificate } from "aws-cdk-lib/aws-certificatemanager";
import { Distribution, IDistribution } from "aws-cdk-lib/aws-cloudfront";
import { RestApiOrigin } from "aws-cdk-lib/aws-cloudfront-origins";
import {
  Alarm,
  CfnAlarm,
  CfnAnomalyDetector,
  ComparisonOperator,
  MathExpression,
  Metric,
  Stats,
} from "aws-cdk-lib/aws-cloudwatch";
import {
  LambdaApplication,
  LambdaDeploymentConfig,
  LambdaDeploymentGroup,
} from "aws-cdk-lib/aws-codedeploy";
import {
  AdotLambdaExecWrapper,
  AdotLambdaLayerGenericVersion,
  AdotLayerVersion,
  Alias,
  Architecture,
  Code,
  Function,
  LambdaInsightsVersion,
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
      runtime: Runtime.PROVIDED_AL2023,
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
      // TODO: document what this is
      // TODO: add URLs that go directly to the appropriate CloudWatch view
      insightsVersion: LambdaInsightsVersion.VERSION_1_0_229_0,
    });

    const codedeployApplication = new LambdaApplication(this, "Lambda", {
      applicationName: "cisserver",
    });

    const alias = new Alias(this, "Prod", {
      aliasName: "prod",
      version: lambda.currentVersion,
    });

    const errorsMetric = lambda.metricErrors();

    const anomalyDetector = new CfnAnomalyDetector(this, "AnomalyDetector", {
      stat: Stats.SUM,
      namespace: errorsMetric.namespace,
      metricName: errorsMetric.metricName,
    });

    const alarm = new CfnAlarm(this, "AnomalyDetectorAlarm", {
      comparisonOperator: ComparisonOperator.GREATER_THAN_THRESHOLD,
      thresholdMetricId: "e1",
      evaluationPeriods: 3,
      metrics: [
        {
          expression: `ANOMALY_DETECTION_BAND(m1, 2)`,
          id: "e1",
        },
        {
          id: "m1",
          metricStat: {
            metric: {
              namespace: errorsMetric.namespace,
              metricName: errorsMetric.metricName,
            },
            period: cdk.Duration.minutes(1).toSeconds(),
            stat: Stats.SUM,
          },
        },
      ],
    });

    const highAlarm = Alarm.fromAlarmArn(this, "HighAlarm", alarm.attrArn);

    const deploymentGroup = new LambdaDeploymentGroup(
      this,
      "LambdaDeploymentGroup",
      {
        application: codedeployApplication,
        alias,
        autoRollback: {
          deploymentInAlarm: true,
          failedDeployment: true,
          stoppedDeployment: true,
        },
        deploymentConfig: LambdaDeploymentConfig.CANARY_10PERCENT_10MINUTES,
        alarms: [highAlarm],
      }
    );

    // We create an API Gateway here because our code only understands API Gateway Proxy requests. If our code understood other types of requests (like normal, unwrapped HTTP requests), we could also skip the API Gateway completely and use Lambda function URLs instead as our CloudFront target.
    const apiGW = new LambdaRestApi(this, "APIGateway", {
      handler: lambda,
      // We need to tell API Gateway that we want tracing so we see the path of an individual request from the gateway to Lambda.
      deployOptions: { tracingEnabled: true },
      // Since we're using CloudFront in front of our API Gateway, it makes architecturally more sense to use regional endpoints here, instead of edge endpoints, since CF already caches at the edge. If we didn't use CloudFront we could use edge Lambdas and enable their own cache. That cache has a hourly cost though, unlike CloudFront, which is purely billed on data transfer and request count.
      endpointTypes: [EndpointType.REGIONAL],
    });

    // We want our users to actually call our Lambda as little as possible, as that will lessen costs both in Lambda availability and cost. We use CloudFront to cache as much as possible so very few requests actually hit the Lambda. We'll need to configure the behavior more here if we have users or other cookie or header based behaviours which mean that we can't just blindly cache based on the URL alone (since the resulting response might be different for each user, and we don't want to show another user's page to any other user).
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
