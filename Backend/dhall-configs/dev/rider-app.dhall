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

let tables =
      { enableKVForWriteAlso =
            [ { nameOfTable = "tag", percentEnable = 100 }
            , { nameOfTable = "on_search_event", percentEnable = 100 }
            , { nameOfTable = "location_backup", percentEnable = 100 }
            , { nameOfTable = "black_list_org", percentEnable = 100 }
            , { nameOfTable = "issue", percentEnable = 100 }
            , { nameOfTable = "product_instance_backup", percentEnable = 100 }
            , { nameOfTable = "quote_bak_1022", percentEnable = 100 }
            , { nameOfTable = "schema_migrations", percentEnable = 100 }
            , { nameOfTable = "ride_booking_bak_1022", percentEnable = 100 }
            , { nameOfTable = "search_request_bak_1022", percentEnable = 100 }
            , { nameOfTable = "beckn_request", percentEnable = 100 }
            , { nameOfTable = "quote_bak_1026", percentEnable = 100 }
            , { nameOfTable = "ride_booking_bak_1026", percentEnable = 100 }
            , { nameOfTable = "search_request_location_1026"
              , percentEnable = 100
              }
            , { nameOfTable = "rental_quote_bak_1027", percentEnable = 100 }
            , { nameOfTable = "quote_terms_bak_1027", percentEnable = 100 }
            , { nameOfTable = "rental_slab", percentEnable = 100 }
            , { nameOfTable = "trip_terms", percentEnable = 100 }
            , { nameOfTable = "fare_breakup", percentEnable = 100 }
            , { nameOfTable = "cancellation_reason", percentEnable = 100 }
            , { nameOfTable = "merchant_service_config", percentEnable = 100 }
            , { nameOfTable = "webengage", percentEnable = 100 }
            , { nameOfTable = "estimate_breakup", percentEnable = 100 }
            , { nameOfTable = "person_flow_status", percentEnable = 100 }
            , { nameOfTable = "search_request_location", percentEnable = 100 }
            , { nameOfTable = "booking_location", percentEnable = 100 }
            , { nameOfTable = "person_default_emergency_number"
              , percentEnable = 100
              }
            , { nameOfTable = "sos", percentEnable = 100 }
            , { nameOfTable = "special_zone_quote", percentEnable = 100 }
            , { nameOfTable = "special_location", percentEnable = 100 }
            , { nameOfTable = "tag_category_mapping", percentEnable = 100 }
            , { nameOfTable = "place_name_cache", percentEnable = 100 }
            , { nameOfTable = "call_status", percentEnable = 100 }
            , { nameOfTable = "registration_token", percentEnable = 100 }
            , { nameOfTable = "callback_request", percentEnable = 100 }
            , { nameOfTable = "booking_cancellation_reason"
              , percentEnable = 100
              }
            , { nameOfTable = "app_installs", percentEnable = 100 }
            , { nameOfTable = "feedback_form", percentEnable = 100 }
            , { nameOfTable = "hot_spot_config", percentEnable = 100 }
            , { nameOfTable = "saved_location", percentEnable = 100 }
            , { nameOfTable = "person_stats", percentEnable = 100 }
            , { nameOfTable = "rating", percentEnable = 100 }
            , { nameOfTable = "disability", percentEnable = 100 }
            , { nameOfTable = "disability_translation", percentEnable = 100 }
            , { nameOfTable = "person_disability", percentEnable = 100 }
            , { nameOfTable = "payment_order", percentEnable = 100 }
            , { nameOfTable = "payment_transaction", percentEnable = 100 }
            , { nameOfTable = "location", percentEnable = 100 }
            , { nameOfTable = "location_mapping", percentEnable = 100 }
            , { nameOfTable = "aadhaar_otp_req", percentEnable = 100 }
            , { nameOfTable = "aadhaar_otp_verify", percentEnable = 100 }
            , { nameOfTable = "aadhaar_verification", percentEnable = 100 }
            , { nameOfTable = "merchant_operating_city", percentEnable = 100 }
            , { nameOfTable = "merchant_service_usage_config"
              , percentEnable = 100
              }
            , { nameOfTable = "merchant_message", percentEnable = 100 }
            , { nameOfTable = "merchant_payment_method", percentEnable = 100 }
            , { nameOfTable = "exophone", percentEnable = 100 }
            , { nameOfTable = "merchant_config", percentEnable = 100 }
            , { nameOfTable = "search_request", percentEnable = 100 }
            , { nameOfTable = "estimate", percentEnable = 100 }
            , { nameOfTable = "quote", percentEnable = 100 }
            , { nameOfTable = "driver_offer", percentEnable = 100 }
            , { nameOfTable = "booking", percentEnable = 100 }
            , { nameOfTable = "ride", percentEnable = 100 }
            , { nameOfTable = "geometry", percentEnable = 100 }
            , { nameOfTable = "person", percentEnable = 100 }
            , { nameOfTable = "issue_category", percentEnable = 100 }
            , { nameOfTable = "issue_option", percentEnable = 100 }
            , { nameOfTable = "comment", percentEnable = 100 }
            , { nameOfTable = "issue_translation", percentEnable = 100 }
            , { nameOfTable = "media_file", percentEnable = 100 }
            , { nameOfTable = "issue_message", percentEnable = 100 }
            , { nameOfTable = "issue_config", percentEnable = 100 }
            , { nameOfTable = "merchant", percentEnable = 100 }
            , { nameOfTable = "issue_report", percentEnable = 100 }
            ]
          : List { nameOfTable : Text, percentEnable : Natural }
      , enableKVForRead =
            [ "tag"
            , "on_search_event"
            , "location_backup"
            , "black_list_org"
            , "issue"
            , "product_instance_backup"
            , "quote_bak_1022"
            , "schema_migrations"
            , "ride_booking_bak_1022"
            , "search_request_bak_1022"
            , "beckn_request"
            , "quote_bak_1026"
            , "ride_booking_bak_1026"
            , "search_request_location_1026"
            , "rental_quote_bak_1027"
            , "quote_terms_bak_1027"
            , "rental_slab"
            , "trip_terms"
            , "fare_breakup"
            , "cancellation_reason"
            , "merchant_service_config"
            , "webengage"
            , "estimate_breakup"
            , "person_flow_status"
            , "search_request_location"
            , "booking_location"
            , "person_default_emergency_number"
            , "sos"
            , "special_zone_quote"
            , "special_location"
            , "tag_category_mapping"
            , "place_name_cache"
            , "call_status"
            , "registration_token"
            , "callback_request"
            , "booking_cancellation_reason"
            , "app_installs"
            , "feedback_form"
            , "hot_spot_config"
            , "saved_location"
            , "person_stats"
            , "rating"
            , "disability"
            , "disability_translation"
            , "person_disability"
            , "payment_order"
            , "payment_transaction"
            , "location"
            , "location_mapping"
            , "aadhaar_otp_req"
            , "aadhaar_otp_verify"
            , "aadhaar_verification"
            , "merchant_operating_city"
            , "merchant_service_usage_config"
            , "merchant_message"
            , "merchant_payment_method"
            , "exophone"
            , "merchant_config"
            , "search_request"
            , "estimate"
            , "quote"
            , "driver_offer"
            , "booking"
            , "ride"
            , "geometry"
            , "person"
            , "issue_category"
            , "issue_option"
            , "comment"
            , "issue_translation"
            , "media_file"
            , "issue_message"
            , "issue_config"
            , "merchant"
            , "issue_report"
            ]
          : List Text
      , kafkaNonKVTables = [] : List Text
      }

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
    , tables
    , dontEnableForDb
    , dontEnableForKafka
    , maxMessages
    , incomingAPIResponseTimeout = +15
    }
