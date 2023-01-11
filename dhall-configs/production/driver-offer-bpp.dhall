let common = ./common.dhall

let sec = ./secrets/driver-offer-bpp.dhall

let esqDBCfg =
      { connectHost = "adb.driver.primary.beckn.juspay.net"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_driver_offer_bpp"
      , connectSchemaName = "atlas_driver_offer_bpp"
      }

let esqDBReplicaCfg =
      { connectHost = "adb.driver.reporting.beckn.juspay.net"
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let slackCfg =
      { channelName = "beckn-driver-onboard-alerts"
      , slackToken = common.slackToken
      }

let driverOnboardingConfigs =
      { onboardingTryLimit = +3
      , onboardingRetryTimeinHours = +24
      , onboardSupportSmsTemplate =
          ''
          Driver Onboarding Alert!!
           Driver is facing following issues while onboarding to ({#org#}).
          Reasons:
           {#reasons#}
          Please contact him +91-{#driver-phone#}.''
      , checkRCInsuranceExpiry = False
      , checkRCExpiry = False
      , checkRCVehicleClass = True
      , checkDLExpiry = False
      , checkDLVehicleClass = True
      , checkImageExtraction = True
      , checkImageExtractionForDashboard = True
      , validDLVehicleClassInfixes =
        [ "AUTORICKSHAW"
        , "LMV"
        , "3W-NT"
        , "3WT"
        , "3W-T"
        , "LIGHT MOTOR VEHICLE"
        , "3W-CAB"
        ]
      }

let rcfg =
      { connectHost = "cache.primary.beckn.juspay.net"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

let smsConfig =
      { sessionConfig = common.smsSessionConfig
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        }
      , useFakeSms = None Natural
      , url = "https://http.myvfirst.com"
      , sender = "JUSPAY"
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +8, limitResetTimeInSec = +40 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let driverPoolCfg =
      { minRadiusOfSearch = +500
      , maxRadiusOfSearch = +1500
      , radiusStepSize = +500
      , driverPositionInfoExpiry = Some +180
      , intelligentPoolPercentage = Some +50
      , actualDistanceThreshold = Some +1900
      , maxDriverQuotesRequired = +1
      , driverQuoteLimit = +2
      }

let overrideDriverPoolCfg =
      [ { configRange = { startDistance = +0, endDistance = None Integer }
        , driverPoolCfg
        }
      ]

let PoolSortingType = < Intelligent | Random >

let driverPoolBatchesCfg =
      { driverBatchSize = +20
      , maxNumberOfBatches = +1
      , minDriverBatchSize = +3
      , poolSortingType = PoolSortingType.Random
      }

let intelligentPoolConfig =
      { minQuotesToQualifyForIntelligentPool = +5
      , minQuotesToQualifyForIntelligentPoolWindowOption =
        { period = +24, periodType = common.periodType.Hours }
      }

let sendSearchRequestJobCfg =
      { driverPoolBatchesCfg, singleBatchProcessTime = +30 }

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , port = +8016
    , metricsPort = +9999
    , hostName = "juspay.in"
    , nwAddress = "https://api.beckn.juspay.in/dobpp/beckn"
    , selfUIUrl = "https://api.beckn.juspay.in/dobpp/ui"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , s3Config = common.s3Config
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , coreVersion = "0.9.3"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/driver-offer-bpp.log", logRawSql = False }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , graceTerminationPeriod = +90
    , registryUrl = common.registryUrl
    , encTools
    , authTokenCacheExpiry = +600
    , minimumDriverRatesCount = +5
    , disableSignatureAuth = False
    , httpClientOptions = common.httpClientOptions
    , apiRateLimitOptions
    , inviteSmsTemplate =
        "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
    , slackCfg
    , driverOnboardingConfigs
    , otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
    , smsCfg = smsConfig
    , searchRequestExpirationSeconds = +120
    , driverQuoteExpirationSeconds = +15
    , driverUnlockDelay = +2
    , idfyCfg = common.idfyCfg
    , defaultPickupLocThreshold = +500
    , defaultDropLocThreshold = +500
    , defaultRideTravelledDistanceThreshold = +700
    , defaultRideTimeEstimatedThreshold = +900
    , defaultWaitingTimeEstimatedThreshold = +3
    , cacheConfig
    , metricsSearchDurationTimeout = +45
    , dashboardToken = sec.dashboardToken
    , driverLocationUpdateRateLimitOptions
    , driverReachedDistance = +100
    , driverLocationUpdateNotificationTemplate =
        "Yatri: Location updates calls are exceeding for driver with {#driver-id#}."
    , cacheTranslationConfig
    , driverPoolCfg
    , intelligentPoolConfig
    , overrideDriverPoolCfg = Some overrideDriverPoolCfg
    , sendSearchRequestJobCfg
    , driverLocationUpdateTopic = "location-updates"
    , kafkaProducerCfg
    }
