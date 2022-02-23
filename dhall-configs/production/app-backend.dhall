let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Regions: List Text>

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
  , credConfig = {
      username = common.smsUserName
    , password = common.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = None Natural
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let sesConfig =
  { issuesConfig = {
      from = "no-reply@juspay.in"
    , to = ["support@supportyatri.freshdesk.com"]
    , replyTo = []: List Text
    , cc = []: List Text
    , region = "eu-west-1"
    , fromArn = Some "arn:aws:ses:eu-west-1:980691203742:identity/no-reply@juspay.in"
    }
  }

let geofencingConfig =
  { origin = GeoRestriction.Regions ["Ernakulam"]
  , destination = GeoRestriction.Regions ["Kerala"]
}

let gwUri = "https://gateway-1.beckn.nsdl.co.in"

let providerUri = "https://api.beckn.juspay.in/transport/v2"

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
  { brokers = ["FIXME"]
  }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, hedisCfg = hcfg
, smsCfg = smsConfig
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, sesCfg = sesConfig
, port = +8013
, metricsPort = +9999
, xProviderUri = providerUri
, hostName = "juspay.in"
, exotelCallbackUrl = "https://api.beckn.juspay.in/bap/v2/"
, bapSelfIds =
  { cabs = "api.beckn.juspay.in/bap/cab/v1"
  , metro = "api.beckn.juspay.in/bap/metro/v1"
  }
, bapSelfURIs =
  { cabs = "https://api.beckn.juspay.in/bap/cab/v1"
  , metro = "https://api.beckn.juspay.in/bap/metro/v1"
  }
, bapSelfUniqueKeyIds =
  { cabs = "3"
  , metro = "4"
  }
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, searchConfirmExpiry = Some +600
, searchRequestExpiry = Some +600
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = Some common.exotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, geofencingConfig = geofencingConfig
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/app-backend.log"}
, googleMapsUrl = "https://maps.googleapis.com/maps/api/"
, googleMapsKey = common.googleMapsKey
, fcmUrl = common.fcmUrl
, graphhopperUrl = common.graphhopperUrl
, metricsSearchDurationTimeout = +45
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = httpClientOptions
, authTokenCacheExpiry = +600
, registryUrl = common.registryUrl
, gatewayUrl = gwUri
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
, encTools = encTools
, kafkaProducerCfg = kafkaProducerCfg
}
