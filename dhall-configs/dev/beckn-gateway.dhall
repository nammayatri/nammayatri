let common = ./common.dhall
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
, selfId = "JUSPAY.BG.1"
, nwAddress = "http://localhost:8015/v1/"  -- public address of a node
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, migrationPath = Some (env:BECKN_GATEWAY_MIGRATION_PATH as Text ? "dev/migrations/beckn-gateway")
, autoMigrate = True
, searchTimeout = None Integer
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, mobilityCoreVersion = "0.8.2"
, mobilityDomainVersion = "0.8.2"
, fmdCoreVersion = "0.9.1"
, localRetailCoreVersion = "0.9.1"
, foodAndBeverageCoreVersion = "0.9.1"
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, httpClientTimoutMs = +2000
}
