let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

let globalCommon = ../generic/common.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_app"
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

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let hcfg =
      { connectHost = rcfg.connectHost
      , connectPort = rcfg.connectPort
      , connectAuth = rcfg.connectAuth
      , connectDatabase = rcfg.connectDatabase
      , connectMaxConnections = rcfg.connectMaxConnections
      , connectMaxIdleTime = rcfg.connectMaxIdleTime
      , connectTimeout = rcfg.connectTimeout
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

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://gye1yw.api.infobip.com"
      , webhookurl = "http://localhost:8013/v2/update/status"
      , sender = "JUSPAY"
      }

let WebengageConfig = { url = "https://st.in.webengage.com" }

let sampleKafkaConfig
    : globalCommon.kafkaConfig
    = { topicName = "rider-app-events-updates", kafkaKey = "rider-app" }

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

let rideConfig =
      { driverReachedDistance = +100, driverOnTheWayNotifyExpiry = +3600 }

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
      }

let kvConfigUpdateFrequency = +10

let dontEnableForDb = [] : List Text

let dontEnableForKafka = [] : List Text

let maxMessages
    : Text
    = "5000"

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = hcfg
    , hedisClusterCfg = hccfg
    , hedisNonCriticalCfg = hcfg
    , hedisNonCriticalClusterCfg = hccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , cutOffNonCriticalHedisCluster = False
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , webengageCfg = WebengageConfig
    , port = +8013
    , metricsPort = +9999
    , hostName = "localhost"
    , nwAddress = "http://localhost:8013/beckn/cab/v1"
    , selfUIUrl = "http://localhost:8013/v2/"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , s3Config = common.s3Config
    , s3PublicConfig = common.s3PublicConfig
    , searchRequestExpiry = Some +600
    , migrationPath = Some
        (env:RIDER_APP_MIGRATION_PATH as Text ? "dev/migrations/rider-app")
    , autoMigrate = True
    , coreVersion = "0.9.4"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/rider-app.log", logRawSql = True }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , internalAPIKey = sec.internalAPIKey
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
    , rideCfg = rideConfig
    , dashboardToken = sec.dashboardToken
    , cacheConfig
    , cacheTranslationConfig
    , cacheFeedbackFormConfig
    , maxEmergencyNumberCount = +3
    , minTripDistanceForReferralCfg = Some +1000
    , enableRedisLatencyLogging = False
    , enablePrometheusMetricLogging = True
    , eventStreamMap = eventStreamMappings
    , kvConfigUpdateFrequency
    , dontEnableForDb
    , dontEnableForKafka
    , maxMessages
    , incomingAPIResponseTimeout = +15
    , internalEndPointMap = common.internalEndPointMap
    }
