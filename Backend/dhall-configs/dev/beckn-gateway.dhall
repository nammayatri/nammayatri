let common = ./common.dhall

let sec = ./secrets/beckn-gateway.dhall

-- Redis pool: 10 for gateway (Tier 2, moderate traffic).
let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +10
      , connectMaxIdleTime = +30
      , connectTimeout = Some +1
      , connectReadOnly = True
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +10
      , connectMaxIdleTime = +30
      , connectTimeout = Some +1
      , connectReadOnly = True
      }

in  { hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , port = +8015
    , metricsPort = +9998
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
