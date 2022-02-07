let common = ./common.dhall
let sec = ./secrets/beckn-transport.dhall

let postgresConfig =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_transporter_v2"
  }

let pgcfg =
  { connTag = "transporterDb"
  , pgConfig = postgresConfig
  , poolConfig = common.defaultPoolConfig
  , schemaName = "atlas_transporter"
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

let appUri = "http://beckn-app-backend-${common.branchName}.atlas:8013/v2"

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
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
, inviteSmsTemplate = "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
, port = +8014
, bgtmPort = +8114
, metricsPort = +9999
, xAppUri = appUri
, hostName = "juspay.in"
, nwAddress = "https://api.sandbox.beckn.juspay.in/latest/bpp/cab/v1"
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, caseExpiry = Some +7200
, fcmJsonPath = common.fcmJsonPath
, exotelCfg = Some common.exotelCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-transport.log"}
, googleMapsUrl = "https://maps.googleapis.com/maps/api/"
, googleMapsKey = common.googleMapsKey
, fcmUrl = common.fcmUrl
, graphhopperUrl = common.graphhopperUrl
, graceTerminationPeriod = +90
, defaultRadiusOfSearch = +5000 -- meters
, driverPositionInfoExpiry = Some +600
, apiRateLimitOptions = apiRateLimitOptions
, httpClientOptions = common.httpClientOptions
, authTokenCacheExpiry = +600
, minimumDriverRatesCount = +5
, recalculateFareEnabled = True
, updateLocationRefreshPeriod = +5
, updateLocationAllowedDelay = +60
, metricsSearchDurationTimeout = +45
, registryUrl = common.registryUrl
, registrySecrets = sec.registrySecrets
, disableSignatureAuth = False
, encTools = encTools
, kafkaProducerCfg = kafkaProducerCfg
}
