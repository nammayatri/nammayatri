let common = ./common.dhall

let sec = ./secrets/beckn-gateway.dhall

let rcfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_PORT ? 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_CLUSTER_PORT ? 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

in  { hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , port = Natural/toInteger (env:SERVICE_PORT ? 8015)
    , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9998)
    , selfId = "JUSPAY.BG.1"
    , hostName = "localhost"
    , authEntity =
      { signingKey = sec.signingKey
      , uniqueKeyId = "juspay-bg-1-key"
      , signatureExpiry = common.signatureExpiry
      }
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/beckn-gateway.log" }
    , graceTerminationPeriod = +90
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , registryUrl = common.nammayatriRegistryConfig.url
    , disableSignatureAuth = False
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , internalEndPointMap = common.internalEndPointMap
    }
