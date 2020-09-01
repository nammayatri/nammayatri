let common = ./generic/common.dhall
let sec = ./secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5435
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_gateway"
  }

let pgcfg =
  { connTag = "gatewayDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_gateway"
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

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, port = +8015
, metricsPort = +9998
, selfId = Some "JUSPAY.BG.1"
, nwAddress = Some "https://localhost/v1/"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, searchTimeout = None Integer
, traceFlag = common.TraceFlag.TRACE_ALL
, logRawSql = True
}
