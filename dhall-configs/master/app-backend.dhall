let common = ./common.dhall
let sec = ./secrets/app-backend.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_app_v2"
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
  , useFakeSms = Some 7891
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let sesConfig =
  { issuesConfig = {
      from = "support@juspay.in"
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

let gwUri = "http://beckn-gateway-${common.branchName}.atlas:8015/v1"

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

let kafkaProducerCfg =
  { brokers = ["beta-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092"]
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
  { cabs = "api.sandbox.beckn.juspay.in/latest/bap/cab/v1"
  , metro = "api.sandbox.beckn.juspay.in/latest/bap/metro/v1"
  }
, bapSelfURIs =
  { cabs = "https://api.sandbox.beckn.juspay.in/latest/bap/cab/v1/"
  , metro = "https://api.sandbox.beckn.juspay.in/latest/bap/metro/v1/"
  }
, bapSelfUniqueKeyIds =
  { cabs = "19"
  , metro = "19"
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
