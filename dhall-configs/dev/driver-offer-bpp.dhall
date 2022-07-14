let common = ./common.dhall
let sec = ./secrets/driver-offer-bpp.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
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

let idfyConfig =
  { idfyUrl :: "http://localhost:4634",
    idfyAccountId :: common.idfyCfg.accountId,
    idfyApiKey :: common.idfyCfg.apiKey
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
, hostName = "localhost"
, nwAddress = "http://localhost:8016"
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, migrationPath = Some (env:BECKN_TRANSPORT_MIGRATION_PATH as Text ? "dev/migrations/driver-offer-bpp")
, autoMigrate = True
, coreVersion = "0.9.3"
, domainVersion = "0.9.3"
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/beckn-transport.log"}
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
, idfyCfg = idfyConfig
, driverPositionInfoExpiry = None Integer
, searchRequestExpirationSeconds = +3600
, defaultRadiusOfSearch = +5000 -- meters
}
