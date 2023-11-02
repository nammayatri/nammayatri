let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

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
      }

let hedisClusterCfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let consumerProperties =
      { groupId = "groupId"
      , brockers = [ "localhost:29092" ]
      , autoCommit = None Integer
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates" ]
      , offsetReset = common.kafkaOffsetResetConfig.Default
      , consumerProperties
      }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

let tables =
      { enableKVForWriteAlso =
          [] : List { nameOfTable : Text, percentEnable : Natural }
      , enableKVForRead = [] : List Text
      , kafkaNonKVTables = [] : List Text
      , kafkaS3Tables = [] : List Text
      }

in  { hedisCfg
    , hedisClusterCfg
    , hedisNonCriticalCfg = hedisCfg
    , hedisNonCriticalClusterCfg = hedisClusterCfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , esqDBCfg
    , esqDBReplicaCfg
    , cacheConfig
    , dumpEvery = +10
    , kafkaConsumerCfg
    , timeBetweenUpdates = +10
    , availabilityTimeWindowOption
    , granualityPeriodType = common.periodType.Hours
    , httpClientOptions = common.httpClientOptions
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers.log", logRawSql = False }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , tables
    , s3Config = None common.S3ConfigType
    }
