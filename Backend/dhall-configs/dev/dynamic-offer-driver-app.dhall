let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let globalCommon = ../generic/common.dhall

let sosAlertsTopicARN = common.sosAlertsTopicARN

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_driver_offer_bpp"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5434
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let esqLocationDBCfg = esqDBCfg

let esqLocationDBRepCfg =
      { connectHost = esqLocationDBCfg.connectHost
      , connectPort = 5434
      , connectUser = esqLocationDBCfg.connectUser
      , connectPassword = esqLocationDBCfg.connectPassword
      , connectDatabase = esqLocationDBCfg.connectDatabase
      , connectSchemaName = esqLocationDBCfg.connectSchemaName
      , connectionPoolCount = esqLocationDBCfg.connectionPoolCount
      }

let kafkaClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_kafka"
      , tls = False
      , retryInterval = [ +0 ]
      }

let driverClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_driver_offer_bpp"
      , tls = False
      , retryInterval = [ +0 ]
      }

let dashboardClickhouseCfg = driverClickhouseCfg

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let smsConfig =
      { sessionConfig = common.smsSessionConfig
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        , token = None Text
        }
      , useFakeSms = Some 7891
      , url = "http://localhost:4343"
      , sender = "JUSPAY"
      }

let sampleKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "dynamic-offer-driver-events-updates"
      , kafkaKey = "dynamic-offer-driver"
      }

let exophoneKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "ExophoneData"
      , kafkaKey = "dynamic-offer-driver-exophone-events"
      }

let eventTrackerKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "EventTracker"
      , kafkaKey = "dynamic-offer-driver-event-tracker-events"
      }

let sampleLogConfig
    : Text
    = "log-stream"

let eventStreamMappings =
      [ { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig = globalCommon.streamConfig.KafkaStream sampleKafkaConfig
        , eventTypes =
          [ globalCommon.eventType.RideCreated
          , globalCommon.eventType.RideStarted
          , globalCommon.eventType.RideEnded
          , globalCommon.eventType.RideCancelled
          , globalCommon.eventType.BookingCreated
          , globalCommon.eventType.BookingCancelled
          , globalCommon.eventType.BookingCompleted
          , globalCommon.eventType.SearchRequest
          , globalCommon.eventType.Quotes
          , globalCommon.eventType.Estimate
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream exophoneKafkaConfig
        , eventTypes = [ globalCommon.eventType.ExophoneData ]
        }
      , { streamName = globalCommon.eventStreamNameType.LOG_STREAM
        , streamConfig = globalCommon.streamConfig.LogStream sampleLogConfig
        , eventTypes =
          [ globalCommon.eventType.RideEnded
          , globalCommon.eventType.RideCancelled
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig = globalCommon.streamConfig.LogStream sampleLogConfig
        , eventTypes =
          [ globalCommon.eventType.RideEnded
          , globalCommon.eventType.RideCancelled
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream eventTrackerKafkaConfig
        , eventTypes = [ globalCommon.eventType.EventTracker ]
        }
      ]

let apiRateLimitOptions = { limit = +20, limitResetTimeInSec = +1 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let slackCfg =
      { channelName = "beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let apiRateLimitOptions = { limit = +20, limitResetTimeInSec = +1 }

let driverLocationUpdateRateLimitOptions =
      { limit = +100, limitResetTimeInSec = +1 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let kvConfigUpdateFrequency = +10

let appBackendBapInternal =
      { name = "APP_BACKEND"
      , url = "http://localhost:8013/"
      , apiKey = sec.appBackendApikey
      , internalKey = sec.internalKey
      }

let registryMap =
      [ { mapKey = "localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a51"
        , mapValue = "http://localhost:8020/"
        }
      , { mapKey = "localhost/beckn/cab/v1/da4e23a5-3ce6-4c37-8b9b-41377c3c1a52"
        , mapValue = "http://localhost:8020/"
        }
      , { mapKey = "JUSPAY.BG.1", mapValue = "http://localhost:8020/" }
      ]

let AllocatorJobType =
      < SendSearchRequestToDriver
      | UnblockDriver
      | UnblockSoftBlockedDriver
      | SoftBlockNotifyDriver
      | SendPDNNotificationToDriver
      | CheckExotelCallStatusAndNotifyBAP
      | MandateExecution
      | CalculateDriverFees
      | OrderAndNotificationStatusUpdate
      | SendOverlay
      | SupplyDemand
      | BadDebtCalculation
      | RetryDocumentVerification
      | SendManualPaymentLink
      | ScheduledRideNotificationsToDriver
      | DriverReferralPayout
      | ScheduledRideAssignedOnUpdate
      | Daily
      | Weekly
      | Monthly
      | Quarterly
      | DailyUpdateTag
      | MonthlyUpdateTag
      | QuarterlyUpdateTag
      | WeeklyUpdateTag
      | FleetAlert
      | SendWebhookToExternal
      | ScheduledFCMS
      >

let jobInfoMapx =
      [ { mapKey = AllocatorJobType.SendSearchRequestToDriver, mapValue = True }
      , { mapKey = AllocatorJobType.UnblockDriver, mapValue = False }
      , { mapKey = AllocatorJobType.UnblockSoftBlockedDriver, mapValue = False }
      , { mapKey = AllocatorJobType.SoftBlockNotifyDriver, mapValue = False }
      , { mapKey = AllocatorJobType.SupplyDemand, mapValue = True }
      , { mapKey = AllocatorJobType.SendPDNNotificationToDriver
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.CheckExotelCallStatusAndNotifyBAP
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.MandateExecution, mapValue = True }
      , { mapKey = AllocatorJobType.CalculateDriverFees, mapValue = True }
      , { mapKey = AllocatorJobType.OrderAndNotificationStatusUpdate
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.SendOverlay, mapValue = True }
      , { mapKey = AllocatorJobType.BadDebtCalculation, mapValue = True }
      , { mapKey = AllocatorJobType.RetryDocumentVerification
        , mapValue = False
        }
      , { mapKey = AllocatorJobType.SendManualPaymentLink, mapValue = True }
      , { mapKey = AllocatorJobType.ScheduledRideNotificationsToDriver
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.ScheduledRideAssignedOnUpdate
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.DriverReferralPayout, mapValue = True }
      , { mapKey = AllocatorJobType.Daily, mapValue = True }
      , { mapKey = AllocatorJobType.Weekly, mapValue = True }
      , { mapKey = AllocatorJobType.Monthly, mapValue = True }
      , { mapKey = AllocatorJobType.Quarterly, mapValue = True }
      , { mapKey = AllocatorJobType.DailyUpdateTag, mapValue = True }
      , { mapKey = AllocatorJobType.MonthlyUpdateTag, mapValue = True }
      , { mapKey = AllocatorJobType.QuarterlyUpdateTag, mapValue = True }
      , { mapKey = AllocatorJobType.WeeklyUpdateTag, mapValue = True }
      , { mapKey = AllocatorJobType.FleetAlert, mapValue = False }
      , { mapKey = AllocatorJobType.SendWebhookToExternal, mapValue = True }
      , { mapKey = AllocatorJobType.ScheduledFCMS, mapValue = True }
      ]

let LocationTrackingeServiceConfig = { url = "http://localhost:8081/" }

let cacConfig =
      { host = "http://localhost:8080"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let cacTenants = [ "dev", "test" ]

let superPositionConfig =
      { host = "http://localhost:8080"
      , interval = 10
      , tenants = [ "dev", "test" ]
      , retryConnection = False
      , enablePolling = True
      , enableSuperPosition = False
      }

let maxMessages
    : Text
    = "5000"

let modelNamesMap =
      [ { mapKey = "MARUTI ALTO (Some random verioning)"
        , mapValue = "MARUTI ALTO"
        }
      , { mapKey = "MARUTI ALTO (another random verioning)"
        , mapValue = "MARUTI ALTO"
        }
      ]

in  { esqDBCfg
    , esqDBReplicaCfg
    , kafkaClickhouseCfg
    , driverClickhouseCfg
    , dashboardClickhouseCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , port = +8016
    , metricsPort = +9997
    , hostName = "localhost"
    , nwAddress = "http://localhost:8016/beckn"
    , selfUIUrl = "http://localhost:8016/ui/"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , s3Config = common.s3Config
    , s3PublicConfig = common.s3PublicConfig
    , migrationPath =
      [ "dev/migrations-read-only/dynamic-offer-driver-app"
      ,   env:DYNAMIC_OFFER_DRIVER_APP_MIGRATION_PATH as Text
        ? "dev/migrations/dynamic-offer-driver-app"
      , "dev/migrations-after-release/dynamic-offer-driver-app"
      ]
    , autoMigrate = True
    , coreVersion = "0.9.4"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/dynamic-offer-driver-app.log"
            , logRawSql = True
            }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , appBackendBapInternal
    , graceTerminationPeriod = +90
    , encTools
    , authTokenCacheExpiry = +600
    , disableSignatureAuth = False
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , apiRateLimitOptions
    , slackCfg
    , jobInfoMapx
    , smsCfg = smsConfig
    , searchRequestExpirationSeconds = +3600
    , driverQuoteExpirationSeconds = +60
    , driverUnlockDelay = +2
    , dashboardToken = sec.dashboardToken
    , cacheConfig
    , metricsSearchDurationTimeout = +45
    , driverLocationUpdateRateLimitOptions
    , cacheTranslationConfig
    , driverLocationUpdateTopic = "location-updates"
    , broadcastMessageTopic = "broadcast-messages"
    , kafkaProducerCfg
    , snapToRoadSnippetThreshold = +300
    , droppedPointsThreshold = +2000
    , osrmMatchThreshold = +1500
    , maxShards = +5
    , maxNotificationShards = +128
    , enableRedisLatencyLogging = False
    , enablePrometheusMetricLogging = True
    , enableAPILatencyLogging = True
    , enableAPIPrometheusMetricLogging = True
    , eventStreamMap = eventStreamMappings
    , kvConfigUpdateFrequency
    , locationTrackingServiceKey = sec.locationTrackingServiceKey
    , schedulerSetName = "Scheduled_Jobs"
    , schedulerType = common.schedulerType.RedisBased
    , ltsCfg = LocationTrackingeServiceConfig
    , modelNamesMap
    , incomingAPIResponseTimeout = +15
    , internalEndPointMap = common.internalEndPointMap
    , _version = "2.0.0"
    , cacConfig
    , cacTenants
    , superPositionConfig
    , maxStraightLineRectificationThreshold = +800
    , singleBatchProcessingTempDelay = +2
    , ondcTokenMap = sec.ondcTokenMap
    , iosValidateEnpoint = "http://localhost:3000/validateIosToken?idToken="
    , quoteRespondCoolDown = +10
    , sosAlertsTopicARN
    , ondcRegistryUrl = common.ondcRegistryUrl
    , ondcGatewayUrl = common.ondcGatewayUrl
    , nyRegistryUrl = common.nyRegistryUrl
    , nyGatewayUrl = common.nyGatewayUrl
    , nammayatriRegistryConfig = common.nammayatriRegistryConfig
    , urlShortnerConfig = common.urlShortnerConfig
    }
