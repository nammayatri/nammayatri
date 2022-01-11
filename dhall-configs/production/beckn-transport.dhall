let common = ./common.dhall
let sec = ./secrets/beckn-transport.dhall

let postgresConfig =
  { connectHost = "adb.primary.beckn.juspay.net"
  , connectPort = 5432
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

let appUri = "http://beckn-app-backend-${common.branchName}.atlas:8013/v2"

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
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
, xGatewaySelector = "JUSPAY.BG.1"
, xAppUri = appUri
, hostName = "juspay.in"
, nwAddress = "https://api.beckn.juspay.in/transport/v1/"
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "juspay-mobility-bpp-1-key-prod"
  , signatureExpiry = common.signatureExpiry
  }
, caseExpiry = Some +7200
, encService = common.passetto
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
}
