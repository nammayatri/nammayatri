let common = ./common.dhall

let sec = ./secrets/driver-offer-bpp.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_driver_offer_bpp_v2"
      , connectSchemaName = "atlas_driver_offer_bpp"
      }

let esqDBReplicaCfg =
      { connectHost =
          "beckn-integ-v2-r1.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
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

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let slackCfg =
      { channelName = "beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let driverOnboardingConfigs =
      { onboardingTryLimit = +3
      , onboardingRetryTimeinHours = +1
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
      , checkDLExpiry = True
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

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +20, limitResetTimeInSec = +40 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let windowOptions = { period = +7, periodType = common.periodType.Days }

let driverPoolCfg =
      { minRadiusOfSearch = +700
      , maxRadiusOfSearch = +1700
      , radiusStepSize = +500
      , driverPositionInfoExpiry = None Integer
      , actualDistanceThreshold = Some +2000
      , maxDriverQuotesRequired = +1
      , driverQuoteLimit = +2
      }

let overrideDriverPoolCfg =
      [ { configRange = { startDistance = +0, endDistance = Some +5000 }
        , driverPoolCfg =
          { minRadiusOfSearch = +250
          , maxRadiusOfSearch = +750
          , radiusStepSize = +250
          , driverPositionInfoExpiry = None Integer
          , actualDistanceThreshold = Some +1000
          , maxDriverQuotesRequired = +1
          , driverQuoteLimit = +2
          }
        }
      , { configRange = { startDistance = +5001, endDistance = Some +13000 }
        , driverPoolCfg =
          { minRadiusOfSearch = +500
          , maxRadiusOfSearch = +1300
          , radiusStepSize = +400
          , driverPositionInfoExpiry = None Integer
          , actualDistanceThreshold = Some +1600
          , maxDriverQuotesRequired = +1
          , driverQuoteLimit = +2
          }
        }
      ]

let PoolSortingType = < Intelligent | Random >

let driverPoolBatchesCfg =
      { driverBatchSize = +5
      , maxNumberOfBatches = +3
      , poolSortingType = PoolSortingType.Random
      }

let sendSearchRequestJobCfg =
      { driverPoolBatchesCfg, singleBatchProcessTime = +30 }

let kafkaProducerCfg = { brokers = [ "kafka.kafka.svc.cluster.local:9092" ] }

in  { esqDBCfg
    , esqDBReplicaCfg
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
    , cacheConfig
    , windowOptions
    , metricsSearchDurationTimeout = +45
    , dashboardToken = sec.dashboardToken
    , driverLocationUpdateRateLimitOptions
    , driverLocationUpdateNotificationTemplate =
        "Yatri: Location updates calls are exceeding for driver with {#driver-id#}."
    , cacheTranslationConfig
    , driverPoolCfg
    , overrideDriverPoolCfg = Some overrideDriverPoolCfg
    , sendSearchRequestJobCfg
    , driverLocationUpdateTopic = "location-updates"
    , kafkaProducerCfg
    }
