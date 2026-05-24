let common = ./common.dhall

let driverAppPort = Natural/show (env:DRIVER_APP_PORT ? 8016)

let kafkaProducerCfg =
      { brokers = [ env:KAFKA_BROKER as Text ? "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

let hedisClusterCfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_CLUSTER_PORT ? 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

in  { kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , port = Natural/toInteger (env:SERVICE_PORT ? 8090)
    , loggerConfig =
            common.loggerConfig
        //  { logRawSql = False, logFilePath = "/tmp/sdk-event-pipeline.log" }
    , hedisClusterCfg
    , graceTerminationPeriod = +90
    , riderSDKEventsKafkaTopic = "rider-sdk-events"
    , driverSDKEventsKafkaTopic = "driver-sdk-events"
    , metroWebviewEventsKafkaTopic = "metro-webview-events"
    , apiRateLimitOptions = { limit = +1, limitResetTimeInSec = +60 }
    , driverAppConfig =
      { url = "http://127.0.0.1:${driverAppPort}/"
      , apiKey = "ae288466-2add-11ee-be56-0242ac120002"
      }
    , riderAppAuthenticationPrefix = "providerPlatform:authTokenCacheKey:"
    , cacheConfig.configsExpTime = +86400
    }
