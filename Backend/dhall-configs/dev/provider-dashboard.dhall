let common = ./common.dhall

let sec = ./secrets/provider-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_bpp_dashboard"
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

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let shareRideApiRateLimitOptions = { limit = +20, limitResetTimeInSec = +60 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

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

let specialZone =
      { name = common.ServerName.SPECIAL_ZONE
      , url = "http://localhost:8032/"
      , token = sec.specialZoneToken
      }

let criticalAPIs =
      { criticalAPIList =
        [ "/dashboard/:merchantId/booking/sync"
        , "/dashboard/:merchantId/ride/sync/"
        , "/dashboard/:merchantId/ride/rideinfo/:rideId/"
        , "/dashboard/:merchantId/volunteer/:bookingOtp/booking/"
        ]
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , port = +8018
    , migrationPath = Some
        (   env:PROVIDER_DASHBOARD_MIGRATION_PATH as Text
          ? "dev/migrations/provider-dashboard"
        )
    , autoMigrate = True
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/provider-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , shareRideApiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , encTools
    , exotelToken = sec.exotelToken
    , dataServers =
      [ driverOfferBpp, driverOfferBppManagement, appBackend, specialZone ]
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , slackToken = sec.slackToken
    , slackChannel = "CXXXXXXXXXF"
    , criticalAPIs
    }
