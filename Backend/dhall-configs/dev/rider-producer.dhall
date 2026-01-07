let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

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
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let hedisCfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let hedisClusterCfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let hedisSecondaryClusterCfg =
      { connectHost = "localhost"
      , connectPort = 30002
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let cacheConfig = { configsExpTime = +86400 }

let cacConfig =
      { host = "http://localhost:8080"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let kvConfigUpdateFrequency = +10

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let inMemConfig = { enableInMem = True, maxInMemSize = +100000000 }

in  { hedisCfg
    , hedisClusterCfg
    , hedisSecondaryClusterCfg
    , hedisNonCriticalCfg = hedisCfg
    , hedisNonCriticalClusterCfg = hedisClusterCfg
    , hedisMigrationStage = False
    , cutOffHedisCluster = False
    , esqDBCfg
    , esqDBReplicaCfg
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/producer.log", prettyPrinting = True }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , waitTimeMilliSec = +1000.0
    , producerTimestampKey = "producerTimestampKey"
    , batchSize = +1
    , streamName = "Available_Jobs_Rider"
    , cacheConfig
    , schedulerSetName = "Scheduled_Jobs_Rider"
    , entryId = "*"
    , reviverInterval = +2
    , reviveThreshold = +2
    , schedulerType = common.schedulerType.RedisBased
    , maxShards = +5
    , metricsPort = +9990
    , kvConfigUpdateFrequency
    , runReviver = True
    , kafkaProducerCfg
    , cacConfig
    , producersPerPod = +5
    , inMemConfig
    }
