let common = ./common.dhall

let sec = ./secrets/provider-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_bpp_dashboard"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5435
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
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

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let becknTransport =
      { name = common.ServerName.BECKN_TRANSPORT
      , url = "http://localhost:8014/"
      , token = sec.becknTransportToken
      }

let driverOfferBpp =
      { name = common.ServerName.DRIVER_OFFER_BPP
      , url = "http://localhost:8016/"
      , token = sec.driverOfferBppToken
      }

let appBackend =
      { name = common.ServerName.APP_BACKEND
      , url = "http://localhost:8013/"
      , token = sec.appBackendToken
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
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
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , encTools
    , exotelToken = sec.exotelToken
    , dataServers = [ becknTransport, driverOfferBpp, appBackend ]
    }
