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

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-gateway-${common.branchName}.atlas"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

let providerUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-transport-${common.branchName}.atlas"
  , baseUrlPort = +8014
  , baseUrlPath = "/v1"
  }

let nsdlUrl =
  { baseUrlScheme = UrlScheme.Https
  , baseUrlHost = "pilot-gateway-1.beckn.nsdl.co.in"
  , baseUrlPort = +443
  , baseUrlPath = ""
  }

in

{ dbCfg = pgcfg
, smsCfg = common.smsConfig
, port = +8013
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "NSDL"
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
}
