let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let genericCommon = ../generic/common.dhall

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

let inMemConfig = { enableInMem = False, maxInMemSize = +100000000 }

let consumerProperties =
      { groupId = "fleet-communication-dispatch"
      , brockers =
        [ "localhost:${Natural/show (env:KAFKA_BROKER_PORT ? 29092)}" ]
      , autoCommit = None Integer
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let kvConfigUpdateFrequency = +10

let kafkaConsumerCfg =
      { topicNames = [ "fleet-communication-dispatch" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

let cacConfig =
      { host = "http://localhost:${Natural/show (env:MOCK_SERVER_PORT ? 8080)}"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

in  { hedisCfg
    , hedisClusterCfg
    , hedisSecondaryClusterCfg
    , hedisNonCriticalCfg = hedisCfg
    , hedisNonCriticalClusterCfg = hedisClusterCfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , esqDBCfg
    , esqDBReplicaCfg
    , cacheConfig
    , dumpEvery = +10
    , kafkaConsumerCfg
    , timeBetweenUpdates = +10
    , availabilityTimeWindowOption
    , granualityPeriodType = common.periodType.Hours
    , httpClientOptions = common.httpClientOptions
    , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9995)
    , encTools = appCfg.encTools
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath =
                "/tmp/kafka-consumers-fleet-communication-dispatch.log"
            , logRawSql = True
            }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , cacConfig
    , kvConfigUpdateFrequency
    , healthCheckAppCfg = None genericCommon.healthCheckAppCfgT
    , kafkaClickhouseCfg
    , serviceClickhouseCfg
    , dashboardClickhouseCfg
    , kafkaProducerCfg
    , secondaryKafkaProducerCfg
    , kafkaReadBatchSize = +10
    , kafkaReadBatchDelay = +10
    , consumerStartTime = None Integer
    , consumerEndTime = None Integer
    , inMemConfig
    , smsCfg = appCfg.smsCfg
    }
