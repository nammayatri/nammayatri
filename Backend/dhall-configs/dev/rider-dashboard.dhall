let common = ./common.dhall

let sec = ./secrets/rider-dashboard.dhall

let riderAppPort = Natural/show (env:RIDER_APP_PORT ? 8013)

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_bap_dashboard"
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

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let shareRideApiRateLimitOptions = { limit = +20, limitResetTimeInSec = +60 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let appBackend =
      { name = common.ServerName.APP_BACKEND
      , url = "http://localhost:${riderAppPort}/"
      , token = sec.appBackendToken
      }

let appBackendManagement =
      { name = common.ServerName.APP_BACKEND_MANAGEMENT
      , url = "http://localhost:${riderAppPort}/"
      , token = sec.appBackendToken
      }

let bharatTaxi =
      { name = common.ServerName.BHARAT_TAXI
      , url = "http://localhost:3000/"
      , token = sec.bharatTaxiToken
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

let rccfgSecondary =
      { connectHost = "localhost"
      , connectPort = env:REDIS_SECONDARY_CLUSTER_PORT ? 30002
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let cacheConfig = { configsExpTime = +86400 }

let cacConfig =
      { host = "http://localhost:${Natural/show (env:MOCK_SERVER_PORT ? 8080)}"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let kafkaProducerCfg =
      { brokers =
        [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

let sendEmailRateLimitOptions = { limit = +100, limitResetTimeInSec = +600 }

let inMemConfig = { enableInMem = False, maxInMemSize = +100000000 }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisSecondaryClusterCfg = rccfgSecondary
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , port = Natural/toInteger (env:SERVICE_PORT ? 8017)
    , migrationPath =
      [   env:RIDER_DASHBOARD_MIGRATION_PATH as Text
        ? "dev/ddl-migrations/rider-dashboard"
      , "dev/seed-migrations/rider-dashboard"
      , "dev/migrations-read-only/rider-dashboard"
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
    , registrationTokenInactivityTimeout = None Integer
    , updateRestrictedBppRoles = [] : List Text
    , sendEmailRateLimitOptions
    , encTools
    , exotelToken = ""
    , dataServers = [ appBackend, appBackendManagement, bharatTaxi ]
    , merchantUserAccountNumber = +100
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , slackToken = sec.slackToken
    , slackChannel = "CXXXXXXXXXF"
    , internalEndPointMap = common.internalEndPointMap
    , cacheConfig
    , kvConfigUpdateFrequency = +60
    , cacConfig
    , internalAuthAPIKey = "ae288466-2add-11ee-be56-0242ac120002"
    , passwordExpiryDays = None Integer
    , enforceStrongPasswordPolicy = False
    , inMemConfig
    , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9991)
    , incomingAPIResponseTimeout = +15
    , is2faMandatory = True
    , twoFaEnforcementDeadlineText = Some "2026-08-04T00:00:00Z"
    , twoFaOtpTTLInSecs = Some +900
    , twoFaMaxOtpVerifyAttempts = Some +5
    , totpStepSize = Some +30
    , totpClockSkew = Some +2
    , twoFaIssuerName = "Control Centre"
    , twoFaExemptRoles = [] : List Text
    }
