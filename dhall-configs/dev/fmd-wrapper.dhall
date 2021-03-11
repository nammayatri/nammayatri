let common = ./common.dhall
let sec = ./secrets/fmd-wrapper.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5436
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
  , dzBPId = "fmd-wrapper.dunzo"
  , dzBPNwAddress = "http://localhost:8018/v1"
  , payee = sec.payee
  , dzTestMode = True
  , dzQuotationTTLinMin = +10
  }


let gwUri = "http://localhost:8015/v1"

let delhiveryConfig =
  { dlUrl = "https://pelorus.delhivery.com"
  , dlTokenUrl = "https://key-cloak.delhivery.com"
  , dlBPId = "fmd-wrapper.delhivery"
  , dlBPNwAddress = "http://localhost:8018/v1"
  , dlPayee = sec.dlPayee
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, port = +8018
, xGatewayUri = gwUri
, xGatewayApiKey = Some "fmd-wrapper-key"
, migrationPath = Some (env:FMD_WRAPPER_MIGRATION_PATH as Text ? "dev/migrations/fmd-wrapper")
, autoMigrate = True
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/fmd-wrapper.log"}
, coreVersion = "0.8.0"
, domainVersion = "0.8.3"
, dzConfig = dunzoConfig
, dlConfig = delhiveryConfig
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, signatureExpiry = common.signatureExpiry
, selfId = "JUSPAY.FMD.UAT.1"
, logContext = [] : List Text
}
