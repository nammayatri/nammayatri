let common = ./common.dhall

let sec = ./secrets/safety-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_safety_dashboard"
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
      , connectReadOnly = True
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let shareRideApiRateLimitOptions = { limit = +20, limitResetTimeInSec = +60 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kvConfigUpdateFrequency = +10

let sendEmailRateLimitOptions = { limit = +3, limitResetTimeInSec = +600 }

let driverOfferBpp =
      { name = common.ServerName.DRIVER_OFFER_BPP
      , url = "http://localhost:8016/"
      , token = sec.driverOfferBppToken
      }

let driverOfferBppManagement =
      { name = common.ServerName.DRIVER_OFFER_BPP_MANAGEMENT
      , url = "http://localhost:8016/"
      , token = sec.driverOfferBppToken
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

let rccfgSecondary =
      { connectHost = "localhost"
      , connectPort = 30002
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let cacheConfig = { configsExpTime = +86400 }

let cacConfig =
      { host = "http://localhost:8080"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let inMemConfig = { enableInMem = True, maxInMemSize = +100000000 }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisSecondaryClusterCfg = rccfgSecondary
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , port = +8025
    , migrationPath =
      [   env:SAFETY_DASHBOARD_MIGRATION_PATH as Text
        ? "dev/migrations-read-only/safety-dashboard"
      ,   env:SAFETY_DASHBOARD_MIGRATION_PATH as Text
        ? "dev/migrations/safety-dashboard"
      ]
    , autoMigrate = True
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/safety-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , shareRideApiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , registrationTokenInactivityTimeout = None Integer
    , updateRestrictedBppRoles = [] : List Text
    , sendEmailRateLimitOptions
    , encTools
    , exotelToken = ""
    , dataServers = [ driverOfferBpp, driverOfferBppManagement ]
    , merchantUserAccountNumber = +5
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , slackToken = sec.slackToken
    , slackChannel = "CXXXXXXXXXF"
    , internalEndPointMap = common.internalEndPointMap
    , cacheConfig
    , cacConfig
    , kvConfigUpdateFrequency
    , kafkaProducerCfg
    , internalAuthAPIKey = "ae288466-2add-11ee-be56-0242ac120002"
    , passwordExpiryDays = None Integer
    , enforceStrongPasswordPolicy = False
    , inMemConfig
    }
