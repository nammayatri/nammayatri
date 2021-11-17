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

let esqDBCfg = 
  { connectHost = postgresConfig.connectHost
  , connectPort = postgresConfig.connectPort
  , connectUser = postgresConfig.connectUser
  , connectPassword = postgresConfig.connectPassword
  , connectDatabase = postgresConfig.connectDatabase
  , connectSchemaName = pgcfg.schemaName
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

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

let coreVersions =
  { mobility = "0.8.2"
  , finalMileDelivery = "0.9.1"
  , localRetail = "0.9.1"
  , foodAndBeverage = "0.9.1"
  }

in

{ dbCfg = pgcfg
, esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8015
, metricsPort = +9998
, selfId = "JUSPAY.BG.1"
, hostName = "localhost"
, nwAddress = "http://localhost:8015/v1/"  -- public address of a node
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, migrationPath = Some (env:BECKN_GATEWAY_MIGRATION_PATH as Text ? "dev/migrations/beckn-gateway")
, autoMigrate = True
, searchTimeout = None Integer
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-gateway.log"}
, coreVersions = coreVersions
, mobilityDomainVersion = "0.8.2"
, signatureExpiry = common.signatureExpiry
, graceTerminationPeriod = +90
, httpClientOptions = httpClientOptions
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
}
