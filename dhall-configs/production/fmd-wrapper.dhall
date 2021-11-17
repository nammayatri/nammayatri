let common = ./common.dhall
let sec = ./secrets/fmd-wrapper.dhall

let postgresConfig =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_fmd_wrapper"
  }

let pgcfg =
  { connTag = "fmdWrapperDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_fmd_wrapper"
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
  { connectHost = "cache.primary.beckn.juspay.net"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let dunzoConfig =
  { dzUrl = "apis-staging.dunzo.in"
  , dzTokenUrl = "http://d4b.dunzodev.in:9016"
  , dzBPId = "fmd-wrapper.dunzo"
  , dzBPNwAddress = "https://api.beckn.juspay.in/fmd/v1/"
  , payee = sec.payee
  , dzTestMode = True
  , dzQuotationTTLinMin = +10
  }

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

in

{ dbCfg = pgcfg
, esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8018
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = Some "fmd-wrapper-key"
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/fmd-wrapper.log"}
, coreVersion = "0.9.1"
, dzConfig = dunzoConfig
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, signatureExpiry = common.signatureExpiry
, selfId = "JUSPAY.DUNZO.FMD.BPP.PROD"
, graceTerminationPeriod = +90
, httpClientOptions = httpClientOptions
, hostName = "localhost"
, nwAddress = "http://localhost:8018/v1/"
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
}
