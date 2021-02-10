let common = ./common.dhall
let sec = ./secrets/beckn-transport.dhall

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
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
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

let appUri = "http://beckn-app-backend-${common.branchName}.atlas:8013/v1"

let nsdlUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"

let SortMode = < ETA | IdleTime >

let driverAllocationConfig =
  { defaultSortMode = SortMode.ETA
  , driverNotificationExpiry = +20
  , rideAllocationExpiry = +180
  , defaultRadiusOfSearch = +1000
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, smsCfg = smsConfig
, port = +8014
, bgtmPort = +8114
, metricsPort = +9999
, xGatewaySelector = Some "NSDL.BG.1"
, xGatewayNsdlUrl = Some nsdlUrl
, xAppUri = appUri
, selfId = "JUSPAY.MOBILITY.PROVIDER.UAT.1"
, nwAddress = "https://api.sandbox.beckn.juspay.in/transport/v1/"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, caseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None common.ExotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.8.2"
, domainVersion = "0.8.2"
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-transport.log"}
, signatureExpiry = common.signatureExpiry
, driverAllocationConfig = driverAllocationConfig
}
