let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = env:DB_PRIMARY_PORT ? 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_driver_offer_bpp"
      , connectionPoolCount = +25
      }

let kafkaConsumerCfg =
      { topicNames = [ "document-audit" ]
      , consumerProperties =
        { groupId = "documentAuditGroup"
        , brockers =
          [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
        , autoCommit = None Integer
        , kafkaCompression = common.kafkaCompression.LZ4
        }
      }

let redisStreamCfg =
      Some
        { streamPrefix = "dynamic-offer-driver-app:audit.events.shard"
        , shardCount = +10
        , consumerGroupName = "document-audit-consumer-group"
        , readBatchSize = +50
        , readBlockMilliseconds = +5000
        , claimMinIdleMs = +60000
        , claimIntervalSeconds = +30
        , maxDeliveries = +5
        , pauseFlagKey = "document-audit-consumer:pause"
        , pauseSleepSeconds = +5
        }

let serviceClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = env:CLICKHOUSE_PORT ? 8123
      , password = sec.clickHousePassword
      , database = "atlas_app"
      , tls = False
      , retryInterval = [ +0 ]
      }

in  { esqDBCfg
    , esqDBReplicaCfg = esqDBCfg
    , hedisCfg = appCfg.hedisCfg
    , ltsRedisCfg = appCfg.hedisCfg
    , secondaryLTSRedisCfg = Some appCfg.hedisCfg
    , hedisClusterCfg = appCfg.hedisClusterCfg
    , hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
    , hedisNonCriticalCfg = appCfg.hedisNonCriticalCfg
    , hedisNonCriticalClusterCfg = appCfg.hedisNonCriticalClusterCfg
    , hedisMigrationStage = appCfg.hedisMigrationStage
    , cutOffHedisCluster = appCfg.cutOffHedisCluster
    , transport = < Kafka | RedisStream >.RedisStream
    , kafkaConsumerCfg
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/document-audit-consumer.log"
            , logRawSql = False
            }
    , cacheConfig = appCfg.cacheConfig
    , cacConfig = appCfg.cacConfig
    , httpClientOptions = appCfg.httpClientOptions
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , healthCheckAppCfg = Some
      { graceTerminationPeriod = +90
      , healthcheckPort = +8125
      , notificationMinDelay = +60000000
      , driverInactiveDelay = +86400
      , smsCfg = appCfg.smsCfg
      , driverInactiveSmsTemplate = "unused-by-document-audit-consumer"
      , driverAllowedDelayForLocationUpdateInSec = +10
      , driverLocationHealthCheckIntervalInSec = +60
      , fcmNofificationSendCount = +2
      , loggerConfig =
              common.loggerConfig
          //  { logFilePath = "/tmp/document-audit-consumer-hc.log" }
      , batchSize = +100
      , numberOfShards = +10
      , enabledMerchantCityIds = [] : List Text
      }
    , kvConfigUpdateFrequency = appCfg.kvConfigUpdateFrequency
    , metricsPort = Natural/toInteger (env:DOC_AUDIT_METRICS_PORT ? 9998)
    , encTools = appCfg.encTools
    , kafkaProducerCfg = appCfg.kafkaProducerCfg
    , secondaryKafkaProducerCfg = Some appCfg.kafkaProducerCfg
    , serviceClickhouseCfg
    , kafkaClickhouseCfg = appCfg.kafkaClickhouseCfg
    , dashboardClickhouseCfg = appCfg.dashboardClickhouseCfg
    , kafkaReadBatchSize = +10
    , kafkaReadBatchDelay = +10
    , consumerStartTime = None Integer
    , consumerEndTime = None Integer
    , inMemConfig = { enableInMem = False, maxInMemSize = +100000000 }
    , smsCfg = appCfg.smsCfg
    , redisStreamCfg
    , ltsCfg = appCfg.ltsCfg
    , eventStreamMap = appCfg.eventStreamMap
    , schedulerSetName = "Document_Audit_Consumer_Jobs"
    , schedulerType = appCfg.schedulerType
    , maxShards = +5
    , jobInfoMap = [] : List { mapKey : Text, mapValue : Bool }
    , blackListedJobs = [] : List Text
    , shortDurationRetryCfg = appCfg.shortDurationRetryCfg
    }
