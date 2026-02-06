let common = ./common.dhall

let secUnified = ./secrets/unified-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = secUnified.dbUserId
      , connectPassword = secUnified.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_dashboard"
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

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let shareRideApiRateLimitOptions = { limit = +20, limitResetTimeInSec = +60 }

let encTools = { service = common.passetto, hashSalt = secUnified.encHashSalt }

let driverOfferBpp =
      { name = common.ServerName.DRIVER_OFFER_BPP
      , url = "http://localhost:8016/"
      , token = secUnified.driverOfferBppToken
      }

let driverOfferBppManagement =
      { name = common.ServerName.DRIVER_OFFER_BPP_MANAGEMENT
      , url = "http://localhost:8016/"
      , token = secUnified.driverOfferBppToken
      }

let appBackend =
      { name = common.ServerName.APP_BACKEND
      , url = "http://localhost:8013/"
      , token = secUnified.appBackendToken
      }

let appBackendManagement =
      { name = common.ServerName.APP_BACKEND_MANAGEMENT
      , url = "http://localhost:8013/"
      , token = secUnified.appBackendToken
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

let specialZone =
      { name = common.ServerName.SPECIAL_ZONE
      , url = "http://localhost:8032/"
      , token = secUnified.specialZoneToken
      }

let bharatTaxi =
      { name = common.ServerName.BHARAT_TAXI
      , url = "http://localhost:3000/"
      , token = secUnified.bharatTaxiToken
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

let sendEmailRateLimitOptions = { limit = +3, limitResetTimeInSec = +600 }

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
    , kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , port = +8021
    , migrationPath =
      [   env:UNIFIED_DASHBOARD_MIGRATION_PATH as Text
        ? "dev/migrations/unified-dashboard"
      , "dev/migrations-read-only/unified-dashboard"
      ]
    , autoMigrate = True
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/unified-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , shareRideApiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , registrationTokenInactivityTimeout = None Integer
    , updateRestrictedBppRoles = [ "FLEET_OWNER", "OPERATOR" ]
    , sendEmailRateLimitOptions
    , encTools
    , exotelToken = secUnified.exotelToken
    , dataServers =
      [ driverOfferBpp
      , driverOfferBppManagement
      , appBackend
      , appBackendManagement
      , specialZone
      , bharatTaxi
      ]
    , merchantUserAccountNumber = +100
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , slackToken = secUnified.slackToken
    , slackChannel = "CXXXXXXXXXF"
    , internalEndPointMap = common.internalEndPointMap
    , cacheConfig
    , cacConfig
    , kvConfigUpdateFrequency = +60
    , internalAuthAPIKey = "ae288466-2add-11ee-be56-0242ac120002"
    , passwordExpiryDays = None Integer
    , enforceStrongPasswordPolicy = False
    , inMemConfig
    }
