let common = ../generic/common.dhall
let sec = ../secrets/app-backend.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app_v2"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_app"
  }

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

let providerUri = "http://beckn-transport-${common.branchName}.atlas:8014/v1"

let nsdlUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"

in

{ dbCfg = pgcfg
, smsCfg = common.smsDevConfig
, port = +8013
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "JUSPAY"
, xGatewayNsdlUrl = Some nsdlUrl
, nsdlUsername = Some sec.nsdlUsername
, nsdlPassword = Some sec.nsdlPassword
, xProviderUri = providerUri
, bapSelfId = Some "JUSPAY.MOBILITY.APP.UAT.1"
, bapNwAddress = Some "https://api.sandbox.beckn.juspay.in/dev/app/v1/"
, searchConfirmExpiry = Some +7200
, searchCaseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = None common.LoggerConfig
}
