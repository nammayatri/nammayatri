let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_app"
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
  , url = "http://localhost:4343"
  , sender = "JUSPAY"
  }

let sesConfig =
  { issuesConfig = {
      from = "no-reply@juspay.in"
    , to = ["support@supportyatri.freshdesk.com"]
    , replyTo = ["support@supportyatri.freshdesk.com"]
    , cc = ["beckn_mobility@juspay.in"]
    , region = "eu-west-1"
    , fromArn = None Text
    }
  }

let geofencingConfig =
{ origin = GeoRestriction.Regions ["Ernakulam"]
, destination = GeoRestriction.Regions ["Ernakulam", "Kerala"]
}

let gwUri = "http://localhost:8015/v1"

let providerUri = "http://localhost:8014/v2"

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let kafkaProducerCfg =
  { brokers = ["localhost:29092"]
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
, hostName = "localhost"
, exotelCallbackUrl = "http://localhost:8013/v2/"
, bapSelfIds =
  { cabs = "JUSPAY.MOBILITY.APP.UAT.1"
  , metro = "JUSPAY.MOBILITY.APP.UAT.2"
  }
, bapSelfURIs =
  { cabs = "http://localhost:8013/cab/v1/"
  , metro = "http://localhost:8013/metro/v1/"
  }
, bapSelfUniqueKeyIds =
  { cabs = "juspay-mobility-bap-1-key"
  , metro = "juspay-mobility-bap-1-key"
  }
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, searchConfirmExpiry = Some +600
, searchRequestExpiry = Some +600
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = Some common.exotelCfg
, migrationPath = Some (env:APP_BACKEND_MIGRATION_PATH as Text ? "dev/migrations/app-backend")
, autoMigrate = True
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, geofencingConfig = geofencingConfig
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/app-backend.log"}
, googleMapsUrl = common.googleMapsUrl
, googleMapsKey = common.googleMapsKey
, fcmUrl = common.fcmUrl
, graphhopperUrl = common.graphhopperUrl
, metricsSearchDurationTimeout = +45
, graceTerminationPeriod = +90
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = common.httpClientOptions
, authTokenCacheExpiry = +600
, registryUrl = common.registryUrl
, gatewayUrl = gwUri
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
, encTools = encTools
, kafkaProducerCfg = kafkaProducerCfg
}
