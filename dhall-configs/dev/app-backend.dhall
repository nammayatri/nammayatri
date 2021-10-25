let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5433
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app"
  }

let pgcfg =
  { connTag = "providerDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_app"
  }

let esqDBCfg =
  { connectHost = postgresConfig.connectHost
  , connectPort = postgresConfig.connectPort
  , connectUser = postgresConfig.connectUser
  , connectPassword = postgresConfig.connectPassword
  , connectDatabase = postgresConfig.connectDatabase
  , connectSchemaName = pgcfg.schemaName
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
{ origin = GeoRestriction.Regions ["Ernakulam", "Kochi"]
, destination = GeoRestriction.Regions ["Kerala", "Kochi"]
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

let kafkaTools = 
  { brokers = ["localhost:29092"]
  , serviceName = "BAP"
  , targetTopic = "beckn_business_events"
  }  
  
in

{ dbCfg = pgcfg
, esqDBCfg = esqDBCfg
, redisCfg = rcfg
, smsCfg = smsConfig
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, sesCfg = sesConfig
, port = +8013
, metricsPort = +9999
, xProviderUri = providerUri
, hostName = "localhost"
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
, googleMapsUrl = "https://maps.googleapis.com/maps/api/"
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
, kafkaToolsConfig = kafkaTools
}
