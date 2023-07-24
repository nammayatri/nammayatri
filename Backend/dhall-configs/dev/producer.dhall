let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let hedisCfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let hedisClusterCfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let cacheConfig = { configsExpTime = +86400 }

in  { hedisCfg
    , hedisClusterCfg
    , hedisNonCriticalCfg = hedisCfg
    , hedisNonCriticalClusterCfg = hedisClusterCfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = False
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/producer.log", prettyPrinting = True }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , batchSize = +1000
    , streamName = "xx"
    , setName = "yy"
    , entryId = "*"
    }
