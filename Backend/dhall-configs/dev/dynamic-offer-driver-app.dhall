let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let globalCommon = ../generic/common.dhall

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
      , host = "xxxxx"
      , port = 1234
      , password = sec.clickHousePassword
      , database = "xxxx"
      , tls = True
      }

let driverClickhouseCfg =
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

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
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
      ]

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let slackCfg =
      { channelName = "beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +100, limitResetTimeInSec = +1 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let tables =
      { enableKVForWriteAlso =
            [ { nameOfTable = "schema_migrations", percentEnable = 100 }
            , { nameOfTable = "business_event", percentEnable = 100 }
            , { nameOfTable = "beckn_request", percentEnable = 100 }
            , { nameOfTable = "fare_policy_27_07_bak", percentEnable = 100 }
            , { nameOfTable = "rating", percentEnable = 100 }
            , { nameOfTable = "operating_city", percentEnable = 100 }
            , { nameOfTable = "image", percentEnable = 100 }
            , { nameOfTable = "driver_license", percentEnable = 100 }
            , { nameOfTable = "geometry", percentEnable = 100 }
            , { nameOfTable = "cancellation_reason", percentEnable = 100 }
            , { nameOfTable = "merchant_service_config", percentEnable = 100 }
            , { nameOfTable = "ride_details", percentEnable = 100 }
            , { nameOfTable = "driver_availability", percentEnable = 100 }
            , { nameOfTable = "driver_flow_status", percentEnable = 100 }
            , { nameOfTable = "merchant_message", percentEnable = 100 }
            , { nameOfTable = "driver_referral", percentEnable = 100 }
            , { nameOfTable = "rider_details", percentEnable = 100 }
            , { nameOfTable = "special_location", percentEnable = 100 }
            , { nameOfTable = "tag_category_mapping", percentEnable = 100 }
            , { nameOfTable = "comment", percentEnable = 100 }
            , { nameOfTable = "issue_translation", percentEnable = 100 }
            , { nameOfTable = "issue_category", percentEnable = 100 }
            , { nameOfTable = "issue_option", percentEnable = 100 }
            , { nameOfTable = "message_report", percentEnable = 100 }
            , { nameOfTable = "search_request_location", percentEnable = 100 }
            , { nameOfTable = "booking_location", percentEnable = 100 }
            , { nameOfTable = "message_translation", percentEnable = 100 }
            , { nameOfTable = "fare_parameters_progressive_details"
              , percentEnable = 100
              }
            , { nameOfTable =
                  "fare_policy_progressive_details_per_extra_km_rate_section"
              , percentEnable = 100
              }
            , { nameOfTable = "fare_policy_driver_extra_fee_bounds"
              , percentEnable = 100
              }
            , { nameOfTable = "fare_parameters", percentEnable = 100 }
            , { nameOfTable = "fare_policy_progressive_details"
              , percentEnable = 100
              }
            , { nameOfTable = "registration_token", percentEnable = 100 }
            , { nameOfTable = "driver_location", percentEnable = 100 }
            , { nameOfTable = "fare_policy_slabs_details_slab"
              , percentEnable = 100
              }
            , { nameOfTable = "fare_policy", percentEnable = 100 }
            , { nameOfTable = "quote_special_zone", percentEnable = 100 }
            , { nameOfTable = "estimate", percentEnable = 100 }
            , { nameOfTable = "search_try", percentEnable = 100 }
            , { nameOfTable = "special_location_priority", percentEnable = 100 }
            , { nameOfTable = "driver_intelligent_pool_config"
              , percentEnable = 100
              }
            , { nameOfTable = "fare_product", percentEnable = 100 }
            , { nameOfTable = "booking_cancellation_reason"
              , percentEnable = 100
              }
            , { nameOfTable = "search_request_special_zone"
              , percentEnable = 100
              }
            , { nameOfTable = "message", percentEnable = 100 }
            , { nameOfTable = "merchant_payment_method", percentEnable = 100 }
            , { nameOfTable = "place_name_cache", percentEnable = 100 }
            , { nameOfTable = "aadhaar_otp_req", percentEnable = 100 }
            , { nameOfTable = "aadhaar_otp_verify", percentEnable = 100 }
            , { nameOfTable = "vehicle_registration_certificate"
              , percentEnable = 100
              }
            , { nameOfTable = "fare_parameters_slab_details"
              , percentEnable = 100
              }
            , { nameOfTable = "leader_board_configs", percentEnable = 100 }
            , { nameOfTable = "onboarding_document_configs"
              , percentEnable = 100
              }
            , { nameOfTable = "bap_metadata", percentEnable = 100 }
            , { nameOfTable = "meta_data", percentEnable = 100 }
            , { nameOfTable = "feedback", percentEnable = 100 }
            , { nameOfTable = "feedback_badge", percentEnable = 100 }
            , { nameOfTable = "aadhaar_verification", percentEnable = 100 }
            , { nameOfTable = "feedback_form", percentEnable = 100 }
            , { nameOfTable = "vehicle", percentEnable = 100 }
            , { nameOfTable = "driver_stats", percentEnable = 100 }
            , { nameOfTable = "driver_block_reason", percentEnable = 100 }
            , { nameOfTable = "driver_rc_association", percentEnable = 100 }
            , { nameOfTable = "call_status", percentEnable = 100 }
            , { nameOfTable = "media_file", percentEnable = 100 }
            , { nameOfTable = "idfy_verification", percentEnable = 100 }
            , { nameOfTable = "mandate", percentEnable = 100 }
            , { nameOfTable = "invoice", percentEnable = 100 }
            , { nameOfTable = "driver_fee", percentEnable = 100 }
            , { nameOfTable = "person", percentEnable = 100 }
            , { nameOfTable = "driver_plan", percentEnable = 100 }
            , { nameOfTable = "plan", percentEnable = 100 }
            , { nameOfTable = "driver_information", percentEnable = 100 }
            , { nameOfTable = "issue_report", percentEnable = 100 }
            , { nameOfTable = "booking", percentEnable = 100 }
            , { nameOfTable = "plan_translation", percentEnable = 100 }
            , { nameOfTable = "payment_order", percentEnable = 100 }
            , { nameOfTable = "payment_transaction", percentEnable = 100 }
            , { nameOfTable = "search_request", percentEnable = 100 }
            , { nameOfTable = "transporter_config", percentEnable = 100 }
            , { nameOfTable = "driver_pool_config", percentEnable = 100 }
            , { nameOfTable = "registry_map_fallback", percentEnable = 100 }
            , { nameOfTable = "merchant", percentEnable = 100 }
            , { nameOfTable = "kiosk_location", percentEnable = 100 }
            , { nameOfTable = "kiosk_location_translation"
              , percentEnable = 100
              }
            , { nameOfTable = "scheduler_job", percentEnable = 100 }
            , { nameOfTable = "driver_home_location", percentEnable = 100 }
            , { nameOfTable = "driver_quote", percentEnable = 100 }
            , { nameOfTable = "location", percentEnable = 100 }
            , { nameOfTable = "driver_go_home_request", percentEnable = 100 }
            , { nameOfTable = "notification", percentEnable = 100 }
            , { nameOfTable = "location_mapping", percentEnable = 100 }
            , { nameOfTable = "volunteer", percentEnable = 100 }
            , { nameOfTable = "fleet_driver_association", percentEnable = 100 }
            , { nameOfTable = "go_home_config", percentEnable = 100 }
            , { nameOfTable = "issue_message", percentEnable = 100 }
            , { nameOfTable = "issue_config", percentEnable = 100 }
            , { nameOfTable = "merchant_operating_city", percentEnable = 100 }
            , { nameOfTable = "search_request_for_driver", percentEnable = 100 }
            , { nameOfTable = "merchant_service_usage_config"
              , percentEnable = 100
              }
            , { nameOfTable = "merchant_overlay", percentEnable = 100 }
            , { nameOfTable = "ride", percentEnable = 100 }
            , { nameOfTable = "exophone", percentEnable = 100 }
            ]
          : List { nameOfTable : Text, percentEnable : Natural }
      , enableKVForRead =
            [ "schema_migrations"
            , "business_event"
            , "beckn_request"
            , "fare_policy_27_07_bak"
            , "rating"
            , "operating_city"
            , "image"
            , "driver_license"
            , "geometry"
            , "cancellation_reason"
            , "merchant_service_config"
            , "ride_details"
            , "driver_availability"
            , "driver_flow_status"
            , "merchant_message"
            , "driver_referral"
            , "rider_details"
            , "special_location"
            , "tag_category_mapping"
            , "comment"
            , "issue_translation"
            , "issue_category"
            , "issue_option"
            , "message_report"
            , "search_request_location"
            , "booking_location"
            , "message_translation"
            , "fare_parameters_progressive_details"
            , "fare_policy_progressive_details_per_extra_km_rate_section"
            , "fare_policy_driver_extra_fee_bounds"
            , "fare_parameters"
            , "fare_policy_progressive_details"
            , "registration_token"
            , "driver_location"
            , "fare_policy_slabs_details_slab"
            , "fare_policy"
            , "quote_special_zone"
            , "estimate"
            , "search_try"
            , "special_location_priority"
            , "driver_intelligent_pool_config"
            , "fare_product"
            , "booking_cancellation_reason"
            , "search_request_special_zone"
            , "message"
            , "merchant_payment_method"
            , "place_name_cache"
            , "aadhaar_otp_req"
            , "aadhaar_otp_verify"
            , "vehicle_registration_certificate"
            , "fare_parameters_slab_details"
            , "leader_board_configs"
            , "onboarding_document_configs"
            , "bap_metadata"
            , "meta_data"
            , "feedback"
            , "feedback_badge"
            , "aadhaar_verification"
            , "feedback_form"
            , "vehicle"
            , "driver_stats"
            , "driver_block_reason"
            , "driver_rc_association"
            , "call_status"
            , "media_file"
            , "idfy_verification"
            , "mandate"
            , "invoice"
            , "driver_fee"
            , "person"
            , "driver_plan"
            , "plan"
            , "driver_information"
            , "issue_report"
            , "booking"
            , "plan_translation"
            , "payment_order"
            , "payment_transaction"
            , "search_request"
            , "transporter_config"
            , "driver_pool_config"
            , "registry_map_fallback"
            , "merchant"
            , "kiosk_location"
            , "kiosk_location_translation"
            , "scheduler_job"
            , "driver_home_location"
            , "driver_quote"
            , "location"
            , "driver_go_home_request"
            , "notification"
            , "location_mapping"
            , "volunteer"
            , "fleet_driver_association"
            , "go_home_config"
            , "issue_message"
            , "issue_config"
            , "merchant_operating_city"
            , "search_request_for_driver"
            , "merchant_service_usage_config"
            , "merchant_overlay"
            , "ride"
            , "exophone"
            ]
          : List Text
      , kafkaNonKVTables = [] : List Text
      }

let dontEnableForDb = [] : List Text

let dontEnableForKafka = [] : List Text

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
      | SendPaymentReminderToDriver
      | UnsubscribeDriverForPaymentOverdue
      | UnblockDriver
      | SendPDNNotificationToDriver
      | MandateExecution
      | CalculateDriverFees
      | OrderAndNotificationStatusUpdate
      | SendOverlay
      >

let jobInfoMapx =
      [ { mapKey = AllocatorJobType.SendSearchRequestToDriver, mapValue = True }
      , { mapKey = AllocatorJobType.SendPaymentReminderToDriver
        , mapValue = False
        }
      , { mapKey = AllocatorJobType.UnsubscribeDriverForPaymentOverdue
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.UnblockDriver, mapValue = False }
      , { mapKey = AllocatorJobType.SendPDNNotificationToDriver
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.MandateExecution, mapValue = True }
      , { mapKey = AllocatorJobType.CalculateDriverFees, mapValue = True }
      , { mapKey = AllocatorJobType.OrderAndNotificationStatusUpdate
        , mapValue = True
        }
      , { mapKey = AllocatorJobType.SendOverlay, mapValue = True }
      ]

let LocationTrackingeServiceConfig = { url = "http://localhost:8081/" }

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
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
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
    , migrationPath = Some
        (   env:DYNAMIC_OFFER_DRIVER_APP_MIGRATION_PATH as Text
          ? "dev/migrations/dynamic-offer-driver-app"
        )
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
    , driverReachedDistance = +100
    , cacheTranslationConfig
    , driverLocationUpdateTopic = "location-updates"
    , broadcastMessageTopic = "broadcast-messages"
    , kafkaProducerCfg
    , snapToRoadSnippetThreshold = +300
    , droppedPointsThreshold = +2000
    , minTripDistanceForReferralCfg = Some +1000
    , maxShards = +5
    , enableRedisLatencyLogging = False
    , enablePrometheusMetricLogging = True
    , enableAPILatencyLogging = True
    , enableAPIPrometheusMetricLogging = True
    , eventStreamMap = eventStreamMappings
    , tables
    , locationTrackingServiceKey = sec.locationTrackingServiceKey
    , schedulerSetName = "Scheduled_Jobs"
    , schedulerType = common.schedulerType.RedisBased
    , ltsCfg = LocationTrackingeServiceConfig
    , dontEnableForDb
    , dontEnableForKafka
    , maxMessages
    , modelNamesMap
    , incomingAPIResponseTimeout = +15
    }
