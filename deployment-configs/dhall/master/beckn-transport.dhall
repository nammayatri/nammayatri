let common = ../generic/common.dhall
let sec = ../secrets/beckn-transport.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_transporter_v2"
  }

let pgcfg =
  { connTag = "transporterDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  }

let rcfg =
  { connectHost = "ec-redis-beta-002.bfw4iw.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let gwUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-gateway-${common.branchName}.atlas"
  , baseUrlPort = +8015
  , baseUrlPath = "/v1"
  }

let appUri =
  { baseUrlScheme = UrlScheme.Http
  , baseUrlHost = "beckn-app-backend-${common.branchName}.atlas"
  , baseUrlPort = +8013
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
, redisCfg = rcfg
, smsCfg = common.smsConfig
, port = +8014
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "JUSPAY"
, xGatewayNsdlUrl = Some nsdlUrl
, nsdlUsername = Some sec.nsdlUsername
, nsdlPassword = Some sec.nsdlUsername
, xAppUri = appUri
, selfId = Some "JUSPAY.MOBILITY.PROVIDER.UAT.1"
, nwAddress = Some "Http://api.sandbox.beckn.juspay.in/dev/transport/v1/"
, caseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
}
