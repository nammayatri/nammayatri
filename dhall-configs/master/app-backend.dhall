let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let esqDBCfg =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app_v2"
  , connectSchemaName = "atlas_app"
  }

let rcfg =
  { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
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
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = Some 7891
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let gwUri = "https://api.sandbox.beckn.juspay.in/dev/gateway/v1"

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let kafkaProducerCfg =
  { brokers = ["alpha-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092"]
  }
let rideConfig =
  {
    driverReachedDistance = +100
  , driverOnTheWayNotifyExpiry = +3600
  }
in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, hedisCfg = hcfg
, smsCfg = smsConfig
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, port = +8013
, metricsPort = +9999
, hostName = "juspay.in"
, selfUIUrl = "https://api.sandbox.beckn.juspay.in/dev/bap/v2/"
, bapSelfIds =
  { cabs = "api.sandbox.beckn.juspay.in/dev/bap/cab/v1"
  , metro = "api.sandbox.beckn.juspay.in/dev/bap/metro/v1"
  }
, bapSelfURIs =
  { cabs = "https://api.sandbox.beckn.juspay.in/dev/bap/cab/v1/"
  , metro = "https://api.sandbox.beckn.juspay.in/dev/bap/metro/v1/"
  }
, bapSelfUniqueKeyIds =
  { cabs = "19"
  , metro = "19"
  }
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
, gatewayUrl = gwUri
, disableSignatureAuth = False
, encTools = encTools
, kafkaProducerCfg = kafkaProducerCfg
, rideCfg = rideConfig
}
