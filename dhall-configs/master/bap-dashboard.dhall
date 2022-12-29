let common = ./common.dhall

let sec = ./secrets/bap-dashboard.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_bap_dashboard_v2"
      , connectSchemaName = "atlas_bap_dashboard"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let rcfg =
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +1
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let appBackend =
      { name = common.ServerName.APP_BACKEND
      , url = "http://beckn-app-backend-master.atlas:8013/"
      , token = sec.appBackendToken
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , port = +8017
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/bap-dashboard.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , authTokenCacheExpiry = +600
    , registrationTokenExpiry = +365
    , encTools
    , dataServers = [ appBackend ]
    }
