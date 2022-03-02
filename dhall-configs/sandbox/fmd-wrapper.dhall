let common = ./common.dhall
let sec = ./secrets/fmd-wrapper.dhall

let postgresConfig =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
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
  { connectHost = "beckn-redis-001-001.zkt6uh.0001.aps1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +2
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let dunzoConfig =
  { dzUrl = "https://apis-staging.dunzo.in/"
  , dzTokenUrl = "https://apis-staging.dunzo.in/"
  , payee = sec.payee
  , dzTestMode = True
  , dzQuotationTTLinMin = +10
  }

let gwUri = "https://api.sandbox.beckn.juspay.in/gateway/v1/"

in

{ dbCfg = pgcfg
, esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8018
, metricsPort = +9999
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/fmd-wrapper.log"}
, coreVersion = "0.9.1"
, dzConfig = dunzoConfig
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "api.sandbox.beckn.juspay.in/fmd/v1"
  , signatureExpiry = common.signatureExpiry
  }
, selfId = "api.sandbox.beckn.juspay.in/fmd/v1"
, graceTerminationPeriod = +90
, httpClientOptions = common.httpClientOptions
, hostName = "api.sandbox.beckn.juspay.in"
, nwAddress = "https://api.sandbox.beckn.juspay.in/fmd/v1/"
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
}
