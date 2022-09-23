let common = ./common.dhall
let sec = ./secrets/bap-dashboard.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_bap_dashboard"
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
  , url = "http://localhost:8013/dashboard/YATRI/"
  , token = sec.appBackendToken
  }

let appBackendArdu =
  { name = ServerName.APP_BACKEND_ARDU
  , url = "http://localhost:8013/dashboard/ARDU/"
  , token = sec.appBackendToken
  }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8017
, migrationPath = Some (env:BAP_DASHBOARD_MIGRATION_PATH as Text ? "dev/migrations/bap-dashboard")
, autoMigrate = True
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/bap-dashboard.log"}
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = common.httpClientOptions
, authTokenCacheExpiry = +600 --seconds
, registrationTokenExpiry = +365 --days
, encTools = encTools
, dataServers = [appBackendYatri, appBackendArdu]
}
