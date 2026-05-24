let common = ./common.dhall

let sec = ./secrets/public-transport-rider-platform.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_public_transport"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

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

let kafkaProducerCfg =
      { brokers = [ env:KAFKA_BROKER as Text ? "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

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

in  { esqDBCfg
    , esqDBReplicaCfg
    , migrationPath =
      [   env:PUBLIC_TRANSPORT_RIDER_PLATFORM_MIGRATION_PATH as Text
        ? "dev/ddl-migrations/public-transport-rider-platform"
      ]
    , autoMigrate = True
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , port = Natural/toInteger (env:SERVICE_PORT ? 8023)
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
    , registryUrl = common.nammayatriRegistryConfig.url
    , kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , internalEndPointMap = common.internalEndPointMap
    }
