let common = ./common.dhall
let sec = ./secrets/bap-dashboard.dhall

let esqDBCfg =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_bap_dashboard"
  , connectSchemaName = "atlas_bap_dashboard"
  }

let rcfg =
  { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +2
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let ServerName = < APP_BACKEND_YATRI | APP_BACKEND_ARDU | BECKN_TRANSPORT | DRIVER_OFFER_BPP >

let appBackendYatri =
  { name = ServerName.APP_BACKEND_YATRI
  , url = "https://api.sandbox.beckn.juspay.in/bap/dashboard/YATRI/"
  , token = sec.appBackendToken
  }

let appBackendArdu =
  { name = ServerName.APP_BACKEND_ARDU
  , url = "https://api.sandbox.beckn.juspay.in/bap/dashboard/ARDU/"
  , token = sec.appBackendToken
  }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8017
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/bap-dashboard.log"}
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = common.httpClientOptions
, authTokenCacheExpiry = +600 --seconds
, registrationTokenExpiry = +365 --days
, encTools = encTools
, dataServers = [appBackendYatri, appBackendArdu]
}
