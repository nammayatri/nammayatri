let common = ./common.dhall
let sec = ./secrets/driver-offer-bpp.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let postgresConfig =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_driver_offer_bpp_v2"
  }

let esqDBCfg =
  { connectHost = postgresConfig.connectHost
  , connectPort = postgresConfig.connectPort
  , connectUser = postgresConfig.connectUser
  , connectPassword = postgresConfig.connectPassword
  , connectDatabase = postgresConfig.connectDatabase
  , connectSchemaName = "atlas_driver_offer_bpp"
  }

let rcfg =
  { connectHost = "beckn-redis-001-001.zkt6uh.0001.aps1.cache.amazonaws.com"
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
  , url = "https://http.myvfirst.com"
  , sender = "JUSPAY"
  }

let geofencingConfig =
{ origin = GeoRestriction.Regions ["Ernakulam"]
, destination = GeoRestriction.Regions ["Ernakulam", "Kerala"]
}

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

in

{ esqDBCfg = esqDBCfg
, redisCfg = rcfg
, port = +8016
, metricsPort = +9997
, hostName = "juspay.in"
, nwAddress = "https://api.sandbox.beckn.juspay.in/dev/dobpp/cab/v1"
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/driver-offer-bpp.log"}
, googleMapsUrl = common.googleMapsUrl
, googleMapsKey = common.googleMapsKey
, graceTerminationPeriod = +90
, registryUrl = common.registryUrl
, encTools = encTools
, authTokenCacheExpiry = +600
, disableSignatureAuth = False
, httpClientOptions = common.httpClientOptions
, fcmUrl = common.fcmUrl
, fcmJsonPath = common.fcmJsonPath
, apiRateLimitOptions = apiRateLimitOptions
, inviteSmsTemplate = "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, smsCfg = smsConfig
, driverPositionInfoExpiry = None Integer
, searchRequestExpirationSeconds = +3600
, driverQuoteExpirationSeconds = +15
, defaultRadiusOfSearch = +5000 -- meters
}
