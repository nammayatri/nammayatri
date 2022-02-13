let common = ./common.dhall
let sec = ./secrets/fmd-wrapper.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
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
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let dunzoConfig =
  { dzUrl = "https://apis-staging.dunzo.in/"
  , dzTokenUrl = "https://apis-staging.dunzo.in/"
  , payee = sec.payee
  , dzTestMode = True
  , dzQuotationTTLinMin = +10
  }


let httpClientOptions =
  { timeoutMs = +10000
  , maxRetries = +3
  }

in

{ dbCfg = pgcfg
, esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8018
, metricsPort = +9995
, migrationPath = Some (env:FMD_WRAPPER_MIGRATION_PATH as Text ? "dev/migrations/fmd-wrapper")
, autoMigrate = True
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/fmd-wrapper.log"}
, coreVersion = "0.9.1"
, dzConfig = dunzoConfig
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "juspay-fmd-1-key"
  , signatureExpiry = common.signatureExpiry
  }
, selfId = "JUSPAY.FMD.UAT.1"
, graceTerminationPeriod = +90
, httpClientOptions = common.httpClientOptions
, hostName = "localhost"
, nwAddress = "http://localhost:8018/v1"
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
}
