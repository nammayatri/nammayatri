let common = ./common.dhall
let sec = ./secrets/beckn-gateway.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
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
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, port = +8015
, metricsPort = +9999
, selfId = "JUSPAY.BG.1"
, nwAddress = "https://api.sandbox.beckn.juspay.in/dev/gateway/v1/"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, searchTimeout = None Integer
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, mobilityCoreVersion = "0.8.2"
, mobilityDomainVersion = "0.8.2"
, fmdCoreVersion = "0.8.0"
, fmdDomainVersion = "0.8.3"
, signatureExpiry = common.signatureExpiry
}
