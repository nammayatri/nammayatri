let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let postgresConfig =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5432
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
  { connectHost = "cache.primary.beckn.juspay.net"
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
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let sesConfig =
  { issuesConfig = {
      from = "no-reply@juspay,in"
    , to = ["support@supportyatri.freshdesk.com"]
    , replyTo = ["support@supportyatri.freshdesk.com"]
    , cc = ["beckn_mobility@juspay.in"]
    , region = "eu-west-1"
    , fromArn = Some "arn:aws:ses:eu-west-1:980691203742:identity/no-reply@juspay.in"
    }
  }

let geofencingConfig =
{ origin = GeoRestriction.Regions ["Ernakulam", "Kochi"]
, destination = GeoRestriction.Regions ["Kerala", "Kochi"]
}

let gwUri = "https://gateway-1.beckn.nsdl.co.in"

let providerUri = "http://beckn-transport-${common.branchName}.atlas:8014/v2"

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
, hostName = "juspay.in"
, bapSelfIds =
  { cabs = "JUSPAY.MOBILITY.APP.UAT.1.PROD"
  , metro = "JUSPAY.MOBILITY.APP.UAT.2.PROD"
  }
, bapSelfURIs =
  { cabs = "https://api.beckn.juspay.in/app/cab/v1/"
  , metro = "https://api.beckn.juspay.in/app/metro/v1/"
  }
, bapSelfUniqueKeyIds =
  { cabs = "juspay-mobility-bap-1-key-prod"
  , metro = "juspay-mobility-bap-1-key-prod"
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
, kafkaToolsConfig = kafkaTools
}
