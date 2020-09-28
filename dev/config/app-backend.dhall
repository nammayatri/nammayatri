let common = ./generic/common.dhall
let sec = ./secrets/app-backend.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5433
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_app"
  }

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = Some 7891
  }

let gwUri = "http://localhost:8015/v1"

let providerUri = "http://localhost:8014/v1"

in

{ dbCfg = pgcfg
, smsCfg = smsConfig
, port = +8013
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "JUSPAY"
, xGatewayNsdlUrl = None Text
, nsdlUsername = None Text
, nsdlPassword = None Text
, xProviderUri = providerUri
, bapSelfId = Some "JUSPAY.MOBILITY.APP.UAT.1"
, bapNwAddress = Some "http://localhost:8013/v1/"
, searchConfirmExpiry = Some +7200
, searchCaseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None common.ExotelCfg
, migrationPath = Some (env:APP_BACKEND_MIGRATION_PATH as Text ? "dev/migrations/app-backend")
, autoMigrate = True
, coreVersion = "0.8.0"
, domainVersion = "0.8.2"
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = None common.LoggerConfig
}
