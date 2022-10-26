let common = ./common.dhall

let sec = ./secrets/driver-offer-bpp.dhall

let GeoRestriction = < Unrestricted | Regions : List Text >

let postgresConfig =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
  , connectPort = 5432
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
  { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let smsConfig =
  { sessionConfig = common.smsSessionConfig
  , credConfig =
    { username = common.smsUserName
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

let slackCfg =
  { channelName = "alert_test_channel"
  , slackToken = common.slackToken
  }

let driverOnboardingConfigs =
  { onboardingTryLimit = +3
  , onboardingRetryTimeinHours = +24
  , onboardSupportSmsTemplate = "Driver Onboarding Alert!!\n Driver is facing following issues while onboarding to ({#org#}).\nReasons:\n {#reasons#}\nPlease contact him +91-{#driver-phone#}."
  , checkRCInsuranceExpiry = False
  , checkRCExpiry = False
  , checkRCVehicleClass = True
  , checkDLExpiry = True
  , checkDLVehicleClass = True
  , checkImageExtraction = True
}

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let cacheConfig =
  { configsExpTime = +86400
  }

in

{ esqDBCfg = esqDBCfg
, hedisCfg = rcfg
, port = +8016
, metricsPort = +9997
, hostName = "juspay.in"
, nwAddress = "https://api.sandbox.beckn.juspay.in/dev/dobpp/beckn"
, selfUIUrl = "https://api.sandbox.beckn.juspay.in/dev/dobpp/ui"
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, s3Config = common.s3Config
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, coreVersion = "0.9.3"
, loggerConfig = common.loggerConfig //  { logFilePath = "/tmp/driver-offer-bpp.log", logRawSql = False }
, updateLocationRefreshPeriod = +1
, updateLocationAllowedDelay = +60
, googleMapsUrl = common.googleMapsUrl
, googleMapsKey = common.googleMapsKey
, graceTerminationPeriod = +90
, registryUrl = common.registryUrl
, encTools = encTools
, authTokenCacheExpiry = +600
, minimumDriverRatesCount = +5
, disableSignatureAuth = False
, httpClientOptions = common.httpClientOptions
, fcmUrl = common.fcmUrl
, fcmJsonPath = common.fcmJsonPath
, fcmTokenKeyPrefix = "FIXME"
, apiRateLimitOptions = apiRateLimitOptions
, inviteSmsTemplate = "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
, slackCfg = slackCfg
, driverOnboardingConfigs = driverOnboardingConfigs
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, smsCfg = smsConfig
, driverPositionInfoExpiry = None Integer
, searchRequestExpirationSeconds = +3600
, driverQuoteExpirationSeconds = +15
, defaultRadiusOfSearch = +5000 -- meters
, driverUnlockDelay = +2 -- seconds
, idfyCfg = common.idfyCfg
, defaultPickupLocThreshold = +500
, defaultDropLocThreshold = +500
, cacheConfig = cacheConfig
, metricsSearchDurationTimeout = +45
}
