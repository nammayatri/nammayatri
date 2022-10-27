let common = ./common.dhall
let sec = ./secrets/driver-offer-bpp.dhall

let GeoRestriction = < Unrestricted | Regions : List Text>

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_driver_offer_bpp"
  }

let esqDBReplicaCfg =
  { connectHost = esqDBCfg.connectHost
  , connectPort = 5435
  , connectUser = esqDBCfg.connectUser
  , connectPassword = esqDBCfg.connectPassword
  , connectDatabase = esqDBCfg.connectDatabase
  , connectSchemaName = esqDBCfg.connectSchemaName
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

let geofencingConfig =
{ origin = GeoRestriction.Regions ["Karnataka"]
, destination = GeoRestriction.Regions ["Karnataka"]
}

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let encTools =
  { service = common.passetto
  , hashSalt = sec.encHashSalt
  }

let slackCfg =
  { channelName = "beckn-driver-onboard-test"
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
  , checkImageExtraction = False -- because mock-idfy can't return proper document numbers
  , checkImageExtractionForDashboard = True
  , validDLVehicleClassInfixes = ["AUTORICKSHAW", "LMV", "3W-NT", "3WT", "3W-T", "LIGHT MOTOR VEHICLE", "3W-CAB"]
}

let apiRateLimitOptions =
  { limit = +4
  , limitResetTimeInSec = +600
  }

let driverLocationUpdateRateLimitOptions = { limit = +100, limitResetTimeInSec = +1 }

let cacheConfig =
  { configsExpTime = +86400
  }

in

{ esqDBCfg = esqDBCfg
, esqDBReplicaCfg = esqDBReplicaCfg
, hedisCfg = rcfg
, port = +8016
, metricsPort = +9997
, hostName = "localhost"
, nwAddress = "http://localhost:8016/beckn"
, selfUIUrl = "http://localhost:8016/ui/"
, signingKey = sec.signingKey
, signatureExpiry = common.signatureExpiry
, s3Config = common.s3Config
, migrationPath = Some (env:DRIVER_OFFER_BPP_MIGRATION_PATH as Text ? "dev/migrations/driver-offer-bpp")
, autoMigrate = True
, coreVersion = "0.9.3"
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/driver-offer-bpp.log", logRawSql = False}
, googleCfg = common.googleCfg
, googleTranslateUrl = common.googleTranslateUrl
, googleTranslateKey = common.googleTranslateKey
, graceTerminationPeriod = +90
, registryUrl = common.registryUrl
, encTools = encTools
, authTokenCacheExpiry = +600
, minimumDriverRatesCount = +5
, disableSignatureAuth = False
, httpClientOptions = common.httpClientOptions
, fcmUrl = common.fcmUrl
, fcmJsonPath = common.fcmJsonPath
, fcmTokenKeyPrefix = "ardu-bpp"
, apiRateLimitOptions = apiRateLimitOptions
, inviteSmsTemplate = "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
, slackCfg = slackCfg
, driverOnboardingConfigs = driverOnboardingConfigs
, otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
, smsCfg = smsConfig
, driverPositionInfoExpiry = Some +36000
, searchRequestExpirationSeconds = +3600
, driverQuoteExpirationSeconds = +60
, defaultRadiusOfSearch = +5000 -- meters
, driverUnlockDelay = +2 -- seconds
, idfyCfg = common.idfyCfg
, dashboardToken = sec.dashboardToken
, defaultPickupLocThreshold = +500
, defaultDropLocThreshold = +500
, defaultRideTravelledDistanceThreshold = +700
, defaultRideTimeEstimatedThreshold = +900 --seconds
, cacheConfig = cacheConfig
, metricsSearchDurationTimeout = +45
, driverPoolLimit = Some +10
, driverLocationUpdateRateLimitOptions
, driverLocationUpdateNotificationTemplate = "Yatri: Location updates calls are exceeding for driver with {#driver-id#}."
, geofencingConfig = geofencingConfig
}
