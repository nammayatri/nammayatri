let common = ./common.dhall

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let hedisClusterCfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

in  { kafkaProducerCfg
    , port = +8090
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
      { url = "http://127.0.0.1:8016/"
      , apiKey = "ae288466-2add-11ee-be56-0242ac120002"
      }
    , riderAppAuthenticationPrefix = "providerPlatform:authTokenCacheKey:"
    , cacheConfig.configsExpTime = +86400
    }
