let common = ./common.dhall

-- Redis pool: 5 for low-traffic services (Tier 3).
let hcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +5
      , connectMaxIdleTime = +30
      , connectTimeout = Some +1
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
      , connectMaxConnections = +5
      , connectMaxIdleTime = +30
      , connectTimeout = Some +1
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
