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

let tables =
      { enableKVForWriteAlso =
            [ { nameOfTable = "search_request_location", percentEnable = 100 }
            , { nameOfTable = "registration_token", percentEnable = 100 }
            , { nameOfTable = "search_request", percentEnable = 100 }
            , { nameOfTable = "person_flow_status", percentEnable = 100 }
            , { nameOfTable = "estimate_breakup", percentEnable = 100 }
            , { nameOfTable = "estimate_breakup", percentEnable = 100 }
            , { nameOfTable = "estimate", percentEnable = 100 }
            , { nameOfTable = "on_search_event", percentEnable = 100 }
            , { nameOfTable = "driver_offer", percentEnable = 100 }
            , { nameOfTable = "quote", percentEnable = 100 }
            , { nameOfTable = "booking_location", percentEnable = 100 }
            , { nameOfTable = "ride", percentEnable = 100 }
            , { nameOfTable = "fare_breakup", percentEnable = 100 }
            , { nameOfTable = "directions_cache", percentEnable = 100 }
            , { nameOfTable = "place_name_cache", percentEnable = 100 }
            , { nameOfTable = "booking_cancellation_reason"
              , percentEnable = 100
              }
            , { nameOfTable = "rental_slab", percentEnable = 100 }
            , { nameOfTable = "special_request_zone", percentEnable = 100 }
            , { nameOfTable = "saved_req_location", percentEnable = 100 }
            , { nameOfTable = "trip_terms", percentEnable = 100 }
            , { nameOfTable = "web_engage", percentEnable = 100 }
            , { nameOfTable = "booking", percentEnable = 100 }
            , { nameOfTable = "beckn_request", percentEnable = 100 }
            ]
          : List { nameOfTable : Text, percentEnable : Natural }
      , enableKVForRead =
            [ "search_request_location"
            , "registration_token"
            , "search_request"
            , "person_flow_status"
            , "estimate_breakup"
            , "estimate_breakup"
            , "estimate"
            , "on_search_event"
            , "driver_offer"
            , "quote"
            , "booking_location"
            , "ride"
            , "fare_breakup"
            , "directions_cache"
            , "place_name_cache"
            , "booking_cancellation_reason"
            , "rental_slab"
            , "special_request_zone"
            , "saved_req_location"
            , "trip_terms"
            , "booking"
            , "web_engage"
            , "beckn_request"
            ]
          : List Text
      }

let dontEnableForDb = [] : List Text

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
    , tables
    , dontEnableForDb
    }
