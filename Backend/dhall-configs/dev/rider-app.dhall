let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

let globalCommon = ../generic/common.dhall

let ondcUrl = "https://analytics-api.aws.ondc.org/v1/api/push-txn-logs"

let sosAlertsTopicARN = common.sosAlertsTopicARN

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_app"
      , connectionPoolCount = +10
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

let hcfg =
      { connectHost = rcfg.connectHost
      , connectPort = rcfg.connectPort
      , connectAuth = rcfg.connectAuth
      , connectDatabase = rcfg.connectDatabase
      , connectMaxConnections = rcfg.connectMaxConnections
      , connectMaxIdleTime = rcfg.connectMaxIdleTime
      , connectTimeout = rcfg.connectTimeout
      , connectReadOnly = True
      }

let ltsRedis =
      { connectHost = "localhost"
      , connectPort = 6379
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

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://gye1yw.api.infobip.com"
      , webhookurl = "http://localhost:8013/v2/update/status"
      , sender = "JUSPAY"
      }

let sampleKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "rider-app-events-updates", kafkaKey = "rider-app" }

let autoCompleteKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "AutoCompleteData"
      , kafkaKey = "rider-app-autocomplete-events"
      }

let marketingParamsKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "MarketingParamsData"
      , kafkaKey = "rider-app-marketing-events"
      }

let marketingParamsPreLoginKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "MarketingParamsPreLoginData"
      , kafkaKey = "rider-app-marketing-events"
      }

let routeDataKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "RouteCollection"
      , kafkaKey = "rider-app-route-data-events"
      }

let exophoneKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "ExophoneData", kafkaKey = "rider-app-exophone-events" }

let sampleLogConfig
    : Text
    = "log-stream"

let samplePrometheusConfig
    : Text
    = "prometheus-stream"

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
      , { streamName = globalCommon.eventStreamNameType.PROMETHEUS_STREAM
        , streamConfig =
            globalCommon.streamConfig.PrometheusStream samplePrometheusConfig
        , eventTypes =
          [ globalCommon.eventType.RideCreated
          , globalCommon.eventType.SearchRequest
          ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream autoCompleteKafkaConfig
        , eventTypes = [ globalCommon.eventType.AutoCompleteData ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream routeDataKafkaConfig
        , eventTypes = [ globalCommon.eventType.RouteCollection ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream marketingParamsKafkaConfig
        , eventTypes = [ globalCommon.eventType.MarketingParamsData ]
        }
      , { streamName = globalCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            globalCommon.streamConfig.KafkaStream
              marketingParamsPreLoginKafkaConfig
        , eventTypes = [ globalCommon.eventType.MarketingParamsPreLoginData ]
        }
      ]

let apiRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let searchRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let slackCfg =
      { channelName = "#beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let cacheFeedbackFormConfig = { configsExpTime = +5184000 }

let hccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let hccfgSecondary =
      { connectHost = "localhost"
      , connectPort = 30002
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let kvConfigUpdateFrequency = +10

let RiderJobType =
      < CheckPNAndSendSMS
      | ScheduledRidePopupToRider
      | ScheduledRideNotificationsToRider
      | ScheduleTagActionNotification
      | SafetyIVR
      | CallPoliceApi
      | SafetyCSAlert
      | CheckExotelCallStatusAndNotifyBPP
      | ExecutePaymentIntent
      | CancelExecutePaymentIntent
      | OtherJobTypes
      | MetroIncentivePayout
      | Daily
      | Weekly
      | Monthly
      | Quarterly
      | DailyUpdateTag
      | WeeklyUpdateTag
      | MonthlyUpdateTag
      | QuarterlyUpdateTag
      | PostRideSafetyNotification
      | UpdateCrisUtsData
      | CheckMultimodalConfirmFail
      | CheckRefundStatus
      | MetroBusinessHour
      | NyRegularMaster
      | NyRegularInstance
      | CrisRecon
      | PaymentOrderStatusCheck
      >

let jobInfoMapx =
      [ { mapKey = RiderJobType.CheckPNAndSendSMS, mapValue = True }
      , { mapKey = RiderJobType.ScheduledRidePopupToRider, mapValue = False }
      , { mapKey = RiderJobType.ScheduledRideNotificationsToRider
        , mapValue = True
        }
      , { mapKey = RiderJobType.ScheduleTagActionNotification
        , mapValue = False
        }
      , { mapKey = RiderJobType.SafetyIVR, mapValue = False }
      , { mapKey = RiderJobType.ExecutePaymentIntent, mapValue = True }
      , { mapKey = RiderJobType.CancelExecutePaymentIntent, mapValue = True }
      , { mapKey = RiderJobType.CallPoliceApi, mapValue = False }
      , { mapKey = RiderJobType.SafetyCSAlert, mapValue = False }
      , { mapKey = RiderJobType.CheckExotelCallStatusAndNotifyBPP
        , mapValue = False
        }
      , { mapKey = RiderJobType.OtherJobTypes, mapValue = False }
      , { mapKey = RiderJobType.MetroIncentivePayout, mapValue = True }
      , { mapKey = RiderJobType.Daily, mapValue = True }
      , { mapKey = RiderJobType.Weekly, mapValue = True }
      , { mapKey = RiderJobType.Monthly, mapValue = True }
      , { mapKey = RiderJobType.Quarterly, mapValue = True }
      , { mapKey = RiderJobType.DailyUpdateTag, mapValue = True }
      , { mapKey = RiderJobType.WeeklyUpdateTag, mapValue = True }
      , { mapKey = RiderJobType.MonthlyUpdateTag, mapValue = True }
      , { mapKey = RiderJobType.QuarterlyUpdateTag, mapValue = True }
      , { mapKey = RiderJobType.PostRideSafetyNotification, mapValue = False }
      , { mapKey = RiderJobType.UpdateCrisUtsData, mapValue = True }
      , { mapKey = RiderJobType.CheckMultimodalConfirmFail, mapValue = True }
      , { mapKey = RiderJobType.CheckRefundStatus, mapValue = True }
      , { mapKey = RiderJobType.MetroBusinessHour, mapValue = True }
      , { mapKey = RiderJobType.NyRegularInstance, mapValue = True }
      , { mapKey = RiderJobType.NyRegularMaster, mapValue = True }
      , { mapKey = RiderJobType.CrisRecon, mapValue = True }
      , { mapKey = RiderJobType.PaymentOrderStatusCheck, mapValue = True }
      ]

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

let LocationTrackingeServiceConfig =
      { url = "http://localhost:8081/", secondaryUrl = None Text }

let kafkaClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_kafka"
      , tls = False
      , retryInterval = [ +0 ]
      }

let riderClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_app"
      , tls = False
      , retryInterval = [ +0 ]
      }

let nearByDriverAPIRateLimitOptions = { limit = +5, limitResetTimeInSec = +30 }

let dashboardClickhouseCfg = riderClickhouseCfg

let tsServiceConfig = { url = "http://0.0.0.0:3001/" }

let inMemConfig = { enableInMem = True, maxInMemSize = +100000000 }

let disableViaPointTimetableCheck = False

let noSignatureSubscribers =
      [ "pre-prod-ondc-ticketing-api-delhi.transportstack.in" ]

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = hcfg
    , hedisClusterCfg = hccfg
    , hedisSecondaryClusterCfg = hccfgSecondary
    , hedisNonCriticalCfg = hcfg
    , hedisNonCriticalClusterCfg = hccfg
    , hedisMigrationStage = False
    , ltsRedis
    , cutOffHedisCluster = False
    , cutOffNonCriticalHedisCluster = True
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , port = +8013
    , metricsPort = +9999
    , hostName = "localhost"
    , nwAddress = "http://localhost:8013/beckn/cab/v1"
    , selfUIUrl = "http://localhost:8013/v2/"
    , selfBaseUrl = "http://localhost:8013/"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , s3Config = common.s3Config
    , s3PublicConfig = common.s3PublicConfig
    , searchRequestExpiry = Some +600
    , migrationPath =
      [ "dev/migrations-read-only/rider-app"
      , "dev/migrations/scheduler"
      , env:RIDER_APP_MIGRATION_PATH as Text ? "dev/migrations/rider-app"
      ]
    , autoMigrate = True
    , coreVersion = "0.9.4"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/rider-app.log", logRawSql = True }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , internalAPIKey = sec.internalAPIKey
    , internalClickhouseAPIKey = sec.internalClickhouseAPIKey
    , metricsSearchDurationTimeout = +45
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , searchRateLimitOptions
    , slackCfg
    , searchLimitExceedNotificationTemplate =
        "Customer with {#cust-id#} is exceeding the search limit."
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , dashboardToken = sec.dashboardToken
    , cacheConfig
    , cacheTranslationConfig
    , cacheFeedbackFormConfig
    , maxEmergencyNumberCount = +3
    , storeRidesTimeLimit = +3600
    , minTripDistanceForReferralCfg = Some +1000
    , enableRedisLatencyLogging = False
    , enablePrometheusMetricLogging = True
    , eventStreamMap = eventStreamMappings
    , kvConfigUpdateFrequency
    , incomingAPIResponseTimeout = +15
    , maxShards = +5
    , jobInfoMapx
    , internalEndPointMap = common.internalEndPointMap
    , schedulerSetName = "Scheduled_Jobs_Rider"
    , schedulerType = common.schedulerType.RedisBased
    , _version = "2.0.0"
    , hotSpotExpiry = +604800
    , cacConfig
    , cacTenants
    , tsServiceConfig
    , superPositionConfig
    , collectRouteData = True
    , kafkaClickhouseCfg
    , riderClickhouseCfg
    , dashboardClickhouseCfg
    , ondcTokenMap = sec.ondcTokenMap
    , iosValidateEnpoint = "http://localhost:3000/validateIosToken?idToken="
    , isMetroTestTransaction = False
    , urlShortnerConfig = common.urlShortnerConfig
    , sosAlertsTopicARN
    , ondcRegistryUrl = common.ondcRegistryUrl
    , ondcGatewayUrl = common.ondcGatewayUrl
    , nyRegistryUrl = common.nyRegistryUrl
    , nyGatewayUrl = common.nyGatewayUrl
    , ltsCfg = LocationTrackingeServiceConfig
    , nammayatriRegistryConfig = common.nammayatriRegistryConfig
    , googleSAPrivateKey = sec.googleSAPrivateKey
    , locationTrackingServiceKey = sec.locationTrackingServiceKey
    , nearByDriverAPIRateLimitOptions
    , inMemConfig
    , disableViaPointTimetableCheck
    , parkingApiKey = sec.parkingApiKey
    , frfsMetricsRateLimitHits = +100
    , frfsMetricsRateLimitWindowSec = +60
    , corporatePartnerApiToken = sec.corporatePartnerApiToken
    , noSignatureSubscribers
    }
