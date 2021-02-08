let common = ./common.dhall
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

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = Some 7891
  }

let appUri = "http://localhost:8013/v1"

let SortMode = < ETA | IdleTime >

let driverAllocationConfig =
  { defaultSortMode = SortMode.ETA
  , driverNotificationExpiry = +20
  , rideAllocationExpiry = +180
  , defaultRadiusOfSearch = 1000
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, smsCfg = smsConfig
, port = +8014
, metricsPort = +9997
, xGatewaySelector = Some "JUSPAY.BG.1"
, xGatewayNsdlUrl = None Text
, xAppUri = appUri
, selfId = "JUSPAY.MOBILITY.PROVIDER.UAT.1"
, nwAddress = "http://localhost:8014/v1/"
, credRegistry = common.credRegistry
, signingKeys = common.signingKeys
, caseExpiry = Some +7200
, cronAuthKey = Some sec.cronAutKey
, encService = common.passetto
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = None common.ExotelCfg
, migrationPath = Some (env:BECKN_TRANSPORT_MIGRATION_PATH as Text ? "dev/migrations/beckn-transport")
, autoMigrate = True
, coreVersion = "0.8.2"
, domainVersion = "0.8.2"
, traceFlag = common.TraceFlag.TRACE_ALL
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-transport.log"}
, signatureExpiry = common.signatureExpiry
, driverAllocationConfig = driverAllocationConfig
}
