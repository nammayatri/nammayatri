let common = ./common.dhall

let sec = ./secrets/rider-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5436
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_bap_dashboard"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5436
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

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let shareRideApiRateLimitOptions = { limit = +20, limitResetTimeInSec = +60 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let appBackend =
      { name = common.ServerName.APP_BACKEND
      , url = "http://localhost:8013/"
      , token = sec.appBackendToken
      }

let appBackendManagement =
      { name = common.ServerName.APP_BACKEND_MANAGEMENT
      , url = "http://localhost:8013/"
      , token = sec.appBackendToken
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

let cacheConfig = { configsExpTime = +86400 }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , kafkaProducerCfg
    , port = +8017
    , migrationPath =
      [   env:RIDER_DASHBOARD_MIGRATION_PATH as Text
        ? "dev/migrations/rider-dashboard"
      ]
    , autoMigrate = True
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/rider-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , shareRideApiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , encTools
    , exotelToken = ""
    , dataServers = [ appBackend, appBackendManagement ]
    , merchantUserAccountNumber = +5
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , slackToken = sec.slackToken
    , slackChannel = "CXXXXXXXXXF"
    , internalEndPointMap = common.internalEndPointMap
    , cacheConfig
    , kvConfigUpdateFrequency = +60
    }
