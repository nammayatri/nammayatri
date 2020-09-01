let common = ./generic/common.dhall
let sec = ./secrets/beckn-transport.dhall

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_transporter"
  }

let pgcfg =
  { connTag = "transporterDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_transporter"
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

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "localhost"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

let appUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "localhost"
  , baseUrlPort = +8013
  , baseUrlPath = "/v1"
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, smsCfg = common.smsConfig
, port = +8014
, metricsPort = +9997
, xGatewayUri = appUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "JUSPAY"
, xGatewayNsdlUrl = None BaseUrl
, nsdlUsername = None Text
, nsdlPassword = None Text
, xAppUri = appUri
, selfId = Some "JUSPAY.MOBILITY.PROVIDER.UAT.1"
, nwAddress = Some "https://localhost/v1/"
, caseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, traceFlag = common.TraceFlag.TRACE_ALL
, logRawSql = True
}
