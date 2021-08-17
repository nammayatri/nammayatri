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
  , url = "http://localhost:4343"
  , sender = "JUSPAY"
  }

let appUri = "http://localhost:8013/v1"

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

in

{ dbCfg = pgcfg
, redisCfg = rcfg
, smsCfg = smsConfig
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, inviteSmsTemplate = "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
, port = +8014
, bgtmPort = +8114
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
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-transport.log"}
, signatureExpiry = common.signatureExpiry
, googleMapsUrl = "https://maps.googleapis.com/maps/api/"
, googleMapsKey = common.googleMapsKey
, fcmUrl = common.fcmUrl
, graphhopperUrl = common.graphhopperUrl
, graceTerminationPeriod = +90
, defaultRadiusOfSearch = +5000 -- meters
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = httpClientOptions
, authTokenCacheExpiry = +600
, minimumDriverRatesCount = +5
, recalculateFareEnabled = True
, updateLocationRefreshPeriod = +5
}
