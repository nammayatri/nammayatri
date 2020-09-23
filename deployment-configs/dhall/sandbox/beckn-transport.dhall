let common = ../generic/common.dhall
let sec = ../secrets/beckn-transport.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
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
  { connectHost = "ec-redis-beta-002.bfw4iw.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = None Natural
  }

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

let appUri = "http://beckn-app-backend-${common.branchName}.atlas:8013/v1"

let nsdlUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, smsCfg = smsConfig
, port = +8014
, metricsPort = +9999
, xGatewayUri = gwUri
, xGatewayApiKey = None Text
, xGatewaySelector = Some "NSDL"
, xGatewayNsdlUrl = Some nsdlUrl
, nsdlUsername = Some sec.nsdlUsername
, nsdlPassword = Some sec.nsdlPassword
, xAppUri = appUri
, selfId = Some "JUSPAY.MOBILITY.PROVIDER.UAT.1"
, nwAddress = Some "https://api.sandbox.beckn.juspay.in/dev/transport/v1/"
, caseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None common.ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = None common.LoggerConfig
}
