let common = ./common.dhall

let sec = ./secrets/public-transport-rider-platform.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_public_transport"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5434
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , migrationPath = Some
        (   env:PUBLIC_TRANSPORT_RIDER_PLATFORM_MIGRATION_PATH as Text
          ? "dev/migrations/public-transport-rider-platform"
        )
    , autoMigrate = True
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , port = +8023
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/public-transport-rider-platform.log" }
    , graceTerminationPeriod = +90
    , selfId = "JUSPAY.PUBLIC_TRANSPORT.APP.UAT.1"
    , selfURI = "http://localhost:8023/beckn"
    , authServiceUrl = common.authServiceUrl
    , authEntity =
      { signingKey = sec.signingKey
      , uniqueKeyId = "juspay-mobility-bap-1-key"
      , signatureExpiry = common.signatureExpiry
      }
    , disableSignatureAuth = False
    , hostName = "localhost"
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , registryUrl = common.registryUrl
    , kafkaProducerCfg
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , aclEndPointMap = common.aclEndPointMap
    }
