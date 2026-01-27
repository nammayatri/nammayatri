let common = ./common.dhall

let hcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let kafkaConsumerCfgs =
      { publicTransportQuotes =
        { brokers = [ "localhost:29092" ]
        , groupId = "publicTransportQuotesGroup"
        , timeoutMilliseconds = +10000
        , kafkaCompression = common.kafkaCompression.LZ4
        }
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

in  { port = +8025
    , graceTerminationPeriod = +90
    , hedisCfg = hcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = hcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , kafkaConsumerCfgs
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/search-result-aggregator.log" }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , hedisSecondaryClusterCfg = rccfg
    }
