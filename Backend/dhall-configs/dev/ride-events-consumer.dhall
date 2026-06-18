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
      , connectPort = esqDBCfg.connectPort
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

let kafkaProducerCfg =
      { brokers =
        [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

let kafkaClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = env:CLICKHOUSE_PORT ? 8123
      , password = sec.clickHousePassword
      , database = "atlas_kafka"
      , tls = False
      , retryInterval = [ +0 ]
      }

let serviceClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = env:CLICKHOUSE_PORT ? 8123
      , password = sec.clickHousePassword
      , database = "atlas_driver_offer_bpp"
      , tls = False
      , retryInterval = [ +0 ]
      }

let dashboardClickhouseCfg = serviceClickhouseCfg

let consumerProperties =
      { groupId = "ride-events-consumer"
      , brockers =
        [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
      , autoCommit = None Integer
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let kafkaConsumerCfg = { topicNames = [] : List Text, consumerProperties }

let cacheConfig = { configsExpTime = +86400 }

let kvConfigUpdateFrequency = +10

let cacConfig =
      { host = "http://localhost:${Natural/show (env:MOCK_SERVER_PORT ? 8080)}"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let inMemConfig = { enableInMem = True, maxInMemSize = +100000000 }

let TransportKind = < Kafka | RedisStream >

let redisStreamCfg =
      Some
        { streamPrefix = ""
        , shardCount = +10
        , consumerGroupName = "ride-events-consumer"
        , readBatchSize = +10
        , readBlockMilliseconds = +5000
        , claimMinIdleMs = +60000
        , claimIntervalSeconds = +30
        , maxDeliveries = +3
        , pauseFlagKey = "ride-events-consumer:pause"
        , pauseSleepSeconds = +1
        }

let financeEventsPublisherCfg = Some { streamPrefix = "", shardCount = +8 }

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

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg
    , ltsRedisCfg = hedisCfg
    , secondaryLTSRedisCfg = Some hedisCfg
    , hedisClusterCfg
    , hedisSecondaryClusterCfg
    , hedisNonCriticalCfg = hedisCfg
    , hedisNonCriticalClusterCfg = hedisClusterCfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , transport = TransportKind.RedisStream
    , kafkaConsumerCfg
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers-ride-events.log"
            , logRawSql = False
            }
    , cacheConfig
    , cacConfig
    , httpClientOptions = common.httpClientOptions
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , healthCheckAppCfg = None genericCommon.healthCheckAppCfgT
    , kvConfigUpdateFrequency
    , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9085)
    , encTools = appCfg.encTools
    , kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , serviceClickhouseCfg
    , kafkaClickhouseCfg
    , dashboardClickhouseCfg
    , kafkaReadBatchSize = +10
    , kafkaReadBatchDelay = +0
    , consumerStartTime = Some +0
    , consumerEndTime = Some +24
    , inMemConfig
    , smsCfg = appCfg.smsCfg
    , redisStreamCfg
    , ltsCfg
    , eventStreamMap
    , schedulerSetName
    , schedulerType
    , maxShards
    , jobInfoMap
    , blackListedJobs
    , shortDurationRetryCfg
    , financeEventsPublisherCfg
    }
