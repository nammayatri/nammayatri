let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

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

let clickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "xxxxx"
      , port = 1234
      , password = sec.clickHousePassword
      , database = "xxxx"
      , tls = True
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
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        }
      , useFakeSms = Some 7891
      , url = "http://localhost:4343"
      , sender = "JUSPAY"
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let slackCfg =
      { channelName = "beckn-driver-onboard-test"
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
      , checkDLExpiry = True
      , checkDLVehicleClass = True
      , checkImageExtraction = False
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

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +100, limitResetTimeInSec = +1 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let cancellationScoreRelatedConfig =
      { popupDelayToAddAsPenalty = Some +5
      , thresholdCancellationScore = Some +40
      , thresholdRidesCount = Some +5
      }

let driverPoolCfg =
      { minRadiusOfSearch = +5000
      , maxRadiusOfSearch = +7000
      , radiusStepSize = +500
      , driverPositionInfoExpiry = Some +36000
      , intelligentPoolPercentage = Some +50
      , actualDistanceThreshold = Some +7000
      , maxDriverQuotesRequired = +1
      , driverQuoteLimit = +2
      , driverRequestCountLimit = +3
      }

let intelligentPoolConfig =
      { minQuotesToQualifyForIntelligentPool = +5
      , minQuotesToQualifyForIntelligentPoolWindowOption =
        { period = +24, periodType = common.periodType.Hours }
      }

let overrideDriverPoolCfg =
      [ { configRange = { startDistance = +0, endDistance = None Integer }
        , driverPoolCfg
        }
      ]

let PoolSortingType = < Intelligent | Random >

let driverPoolBatchesCfg =
      { driverBatchSize = +5
      , minDriverBatchSize = +3
      , maxNumberOfBatches = +3
      , poolSortingType = PoolSortingType.Intelligent
      }

let sendSearchRequestJobCfg =
      { driverPoolBatchesCfg, singleBatchProcessTime = +10 }

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

let endRideDefCfg =
      { pickupLocThreshold = +500
      , dropLocThreshold = +500
      , rideTimeEstimatedThreshold = +900
      , waitingTimeEstimatedThreshold = +3
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , clickhouseCfg
    , hedisCfg = rcfg
    , port = +8016
    , metricsPort = +9997
    , hostName = "localhost"
    , nwAddress = "http://localhost:8016/beckn"
    , selfUIUrl = "http://localhost:8016/ui/"
    , signingKey = sec.signingKey
    , defaultPopupDelay = +0
    , signatureExpiry = common.signatureExpiry
    , s3Config = common.s3Config
    , migrationPath = Some
        (   env:DYNAMIC_OFFER_DRIVER_APP_MIGRATION_PATH as Text
          ? "dev/migrations/dynamic-offer-driver-app"
        )
    , autoMigrate = True
    , coreVersion = "0.9.3"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/dynamic-offer-driver-app.log"
            , logRawSql = False
            }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , graceTerminationPeriod = +90
    , registryUrl = common.registryUrl
    , encTools
    , authTokenCacheExpiry = +600
    , minimumDriverRatesCount = +5
    , disableSignatureAuth = False
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , apiRateLimitOptions
    , slackCfg
    , driverOnboardingConfigs
    , smsCfg = smsConfig
    , searchRequestExpirationSeconds = +3600
    , driverQuoteExpirationSeconds = +60
    , driverUnlockDelay = +2
    , idfyCfg = common.idfyCfg
    , dashboardToken = sec.dashboardToken
    , defaultEndRideCfg = endRideDefCfg
    , cacheConfig
    , metricsSearchDurationTimeout = +45
    , driverLocationUpdateRateLimitOptions
    , driverReachedDistance = +100
    , cacheTranslationConfig
    , driverPoolCfg
    , intelligentPoolConfig
    , cancellationScoreRelatedConfig
    , overrideDriverPoolCfg = Some overrideDriverPoolCfg
    , sendSearchRequestJobCfg
    , driverLocationUpdateTopic = "location-updates"
    , broadcastMessageTopic = "broadcast-messages"
    , kafkaProducerCfg
    , maxParallelSearchRequests = +3
    , snapToRoadSnippetThreshold = +300
    , mediaFileUrlPattern =
        "http://localhost:8016/ui/message/media/?filePath=<FILE_PATH>"
    , minTripDistanceForReferralCfg = Some +1000
    , searchRepeatLimit = +1
    }
