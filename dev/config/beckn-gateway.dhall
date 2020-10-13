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
, nwAddress = Some "http://localhost:8015/v1/"  -- public address of a node
, migrationPath = Some (env:BECKN_GATEWAY_MIGRATION_PATH as Text ? "dev/migrations/beckn-gateway")
, autoMigrate = True
, searchTimeout = None Integer
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = None common.LoggerConfig
, mobilityCoreVersion = "0.8.0"
, mobilityDomainVersion = "0.8.2"
, fmdCoreVersion = "0.8.0"
, fmdDomainVersion = "0.8.3"
}
