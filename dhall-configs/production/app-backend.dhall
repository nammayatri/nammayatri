let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let esqDBCfg =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app"
  , connectSchemaName = "atlas_app"
  }

let rcfg =
  { connectHost = "cache.primary.beckn.juspay.net"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let hcfg =
  { connectHost = rcfg.connectHost
  , connectPort = rcfg.connectPort
  , connectAuth = rcfg.connectAuth
  , connectDatabase = rcfg.connectDatabase
  , connectMaxConnections = rcfg.connectMaxConnections
  , connectMaxIdleTime = rcfg.connectMaxIdleTime
  , connectTimeout = rcfg.connectTimeout
  }

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig =
    { username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = None Natural
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let gwUri = "https://api.beckn.juspay.in/gateway/v1"

let nsdlGwUri = "https://gateway-1.beckn.nsdl.co.in"

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let httpClientOptions = { timeoutMs = +2000, maxRetries = +3 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [] : List Text }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, hedisCfg = hcfg
, smsCfg = smsConfig
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, port = +8013
, metricsPort = +9999
, hostName = "juspay.in"
, selfUIUrl = "https://api.beckn.juspay.in/bap/v2/"
, bapSelfIds =
  { cabs = "api.beckn.juspay.in/bap/cab/v1"
  , metro = "api.beckn.juspay.in/bap/metro/v1"
  }
, bapSelfURIs =
  { cabs = "https://api.beckn.juspay.in/bap/cab/v1"
  , metro = "https://api.beckn.juspay.in/bap/metro/v1"
  }
, bapSelfUniqueKeyIds = { cabs = "3", metro = "4" }
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, searchRequestExpiry = Some +600
, exotelCfg = Some common.exotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.9.3"
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/app-backend.log"}
, googleMapsUrl = "https://maps.googleapis.com/maps/api/"
, googleMapsKey = common.googleMapsKey
, metricsSearchDurationTimeout = +45
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = httpClientOptions
, authTokenCacheExpiry = +600
, registryUrl = common.registryUrl
, gatewayUrl = nsdlGwUri
, disableSignatureAuth = False
, encTools = encTools
, kafkaProducerCfg = kafkaProducerCfg
}
