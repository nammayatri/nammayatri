let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let genericCommon = ../generic/common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let ltsPort = Natural/show (env:LOCATION_TRACKING_SERVICE_PORT ? 8081)

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_driver_offer_bpp"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let hedisCfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_PORT ? 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let hedisClusterCfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_CLUSTER_PORT ? 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let hedisSecondaryClusterCfg =
      { connectHost = "localhost"
      , connectPort = env:REDIS_SECONDARY_CLUSTER_PORT ? 30002
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let consumerProperties =
      { groupId = "groupId"
      , brockers =
        [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
      , autoCommit = None Integer
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

let kvConfigUpdateFrequency = +10

let enabledMerchantCityIds = [] : List Text

let healthCheckAppCfg =
      { graceTerminationPeriod = appCfg.graceTerminationPeriod
      , healthcheckPort = +8115
      , notificationMinDelay = +60000000
      , driverInactiveDelay = +86400
      , smsCfg = appCfg.smsCfg
      , driverInactiveSmsTemplate =
          "Alert! You have been marked Busy on Namma Yatri Partner, as we have not received any location update from your phone in more than a day. Please open the app and update your location for the app to work properly."
      , driverAllowedDelayForLocationUpdateInSec = +10
      , driverLocationHealthCheckIntervalInSec = +60
      , fcmNofificationSendCount = +2
      , loggerConfig =
              appCfg.loggerConfig
          //  { logFilePath = "/tmp/driver-tracking-healthcheck.log" }
      , batchSize = +100
      , numberOfShards = +10
      , enabledMerchantCityIds
      }

let kafkaClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = env:CLICKHOUSE_PORT ? 8123
      , password = sec.clickHousePassword
      , database = "atlas_kafka"
      , tls = False
      , retryInterval = [ +0 ]
      }

let kafkaProducerCfg =
      { brokers =
        [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

let serviceClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = env:CLICKHOUSE_PORT ? 8123
      , password = sec.clickHousePassword
      , database = "atlas_app"
      , tls = False
      , retryInterval = [ +0 ]
      }

let dashboardClickhouseCfg = serviceClickhouseCfg

let cacConfig =
      { host = "http://localhost:${Natural/show (env:MOCK_SERVER_PORT ? 8080)}"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let inMemConfig = { enableInMem = False, maxInMemSize = +100000000 }

let TransportKind = < Kafka | RedisStream >

let ltsCfg = { url = "http://localhost:${ltsPort}/", secondaryUrl = None Text }

let sampleKafkaConfig
    : genericCommon.kafkaConfig
    = { topicName = "dynamic-offer-driver-events-updates"
      , kafkaKey = "dynamic-offer-driver"
      }

let eventStreamMap =
      [ { streamName = genericCommon.eventStreamNameType.KAFKA_STREAM
        , streamConfig =
            genericCommon.streamConfig.KafkaStream sampleKafkaConfig
        , eventTypes =
          [ genericCommon.eventType.RideCreated
          , genericCommon.eventType.RideEnded
          ]
        }
      ]

let schedulerSetName = "Scheduled_Jobs"

let schedulerType = common.schedulerType.RedisBased

let maxShards = +5

let jobInfoMap = [] : List { mapKey : Text, mapValue : Bool }

let blackListedJobs = [] : List Text

let shortDurationRetryCfg = genericCommon.shortDurationRetryCfg

in  { hedisCfg
    , hedisClusterCfg
    , hedisNonCriticalCfg = hedisCfg
    , hedisNonCriticalClusterCfg = hedisClusterCfg
    , ltsRedisCfg = hedisCfg
    , secondaryLTSRedisCfg = Some hedisCfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , esqDBCfg
    , esqDBReplicaCfg
    , cacheConfig
    , transport = TransportKind.Kafka
    , kafkaConsumerCfg
    , httpClientOptions = common.httpClientOptions
    , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9994)
    , encTools = appCfg.encTools
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers.log", logRawSql = False }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , kvConfigUpdateFrequency
    , healthCheckAppCfg = Some healthCheckAppCfg
    , cacConfig
    , kafkaClickhouseCfg
    , serviceClickhouseCfg
    , kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , kafkaReadBatchDelay = +10
    , kafkaReadBatchSize = +10
    , consumerStartTime = Some +14
    , consumerEndTime = Some +20
    , dashboardClickhouseCfg
    , inMemConfig
    , hedisSecondaryClusterCfg
    , smsCfg = appCfg.smsCfg
    , redisStreamCfg =
        None
          { streamPrefix : Text
          , shardCount : Integer
          , consumerGroupName : Text
          , readBatchSize : Integer
          , readBlockMilliseconds : Integer
          , claimMinIdleMs : Integer
          , claimIntervalSeconds : Integer
          , maxDeliveries : Integer
          , pauseFlagKey : Text
          , pauseSleepSeconds : Integer
          }
    , ltsCfg
    , eventStreamMap
    , schedulerSetName
    , schedulerType
    , maxShards
    , jobInfoMap
    , blackListedJobs
    , shortDurationRetryCfg
    , financeEventsPublisherCfg = Some { streamPrefix = "", shardCount = +8 }
    }
