let common = ./common.dhall

let sec = ./secrets/bpp-dashboard.dhall

let esqDBCfg =
      { connectHost = "adb.primary.beckn.juspay.net"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_bpp_dashboard"
      , connectSchemaName = "atlas_bpp_dashboard"
      }

let esqDBReplicaCfg =
      { connectHost = "adb.reporting.beckn.juspay.net"
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let rcfg =
      { connectHost = "cache.primary.beckn.juspay.net"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +1
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let becknTransport =
      { name = common.ServerName.BECKN_TRANSPORT
      , url = "https://api.beckn.juspay.in/bpp/"
      , token = sec.becknTransportToken
      }

let driverOfferBpp =
      { name = common.ServerName.DRIVER_OFFER_BPP
      , url = "https://api.beckn.juspay.in/dobpp/"
      , token = sec.driverOfferBppToken
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , port = +8018
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/bpp-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , encTools
    , dataServers = [ becknTransport, driverOfferBpp ]
    }
