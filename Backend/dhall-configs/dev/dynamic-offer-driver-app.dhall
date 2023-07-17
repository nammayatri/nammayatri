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
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5434
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let esqLocationDBCfg =
      { connectHost = "localhost"
      , connectPort = 5454
      , connectUser = sec.locDBUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev_loc"
      , connectSchemaName = "atlas_person_location"
      }

let esqLocationDBRepCfg =
      { connectHost = esqLocationDBCfg.connectHost
      , connectPort = 5454
      , connectUser = esqLocationDBCfg.connectUser
      , connectPassword = esqLocationDBCfg.connectPassword
      , connectDatabase = esqLocationDBCfg.connectDatabase
      , connectSchemaName = esqLocationDBCfg.connectSchemaName
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

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

let tables = { kVTables = [] : List Text, kVHardKilledTables = [] : List Text }

in  { esqDBCfg
    , esqDBReplicaCfg
    , esqLocationDBCfg
    , esqLocationDBRepCfg
    , clickhouseCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
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
    , coreVersion = "0.9.3"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/dynamic-offer-driver-app.log"
            , logRawSql = True
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
    , minTripDistanceForReferralCfg = Some +1000
    , maxShards = +5
    , enableRedisLatencyLogging = False
    , enablePrometheusMetricLogging = True
    , enableAPILatencyLogging = True
    , enableAPIPrometheusMetricLogging = True
    , eventStreamMap = eventStreamMappings
    , tables
    }
