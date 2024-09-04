let globalCommon = ../generic/common.dhall

let common = ./common.dhall

let sec = ./secrets/yudhishthira.dhall

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

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "yudhishthira"
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

let driverClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_driver_offer_bpp"
      , tls = False
      }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = globalCommon.kafkaCompression.LZ4
      }

let cacConfig =
      { host = "http://localhost:8080"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let schedulerConfig =
      { loggerConfig =
              common.loggerConfig
          //  { logRawSql = True
              , logFilePath = "/tmp/kaal-chakra-allocator.log"
              , prettyPrinting = True
              , level = common.LogLevel.ERROR
              }
      , esqDBCfg
      , metricsPort = +8057
      , hedisCfg = rcfg
      , hedisClusterCfg = rccfg
      , hedisNonCriticalCfg = rcfg
      , hedisNonCriticalClusterCfg = rccfg
      , hedisMigrationStage = False
      , cutOffHedisCluster = True
      , hedisPrefix = "kaal-chakra"
      , port = +8059
      , loopIntervalSec = +5
      , expirationTime = +60
      , waitBeforeRetry = +1
      , tasksPerIteration = +20
      , graceTerminationPeriod = +10
      , enableRedisLatencyLogging = False
      , enablePrometheusMetricLogging = True
      , groupName = "myGroup"
      , schedulerType = globalCommon.schedulerType.RedisBased
      , schedulerSetName = "Scheduled_Chakras"
      , streamName = "Available_Chakras"
      , maxThreads = +10
      , block = +10000
      , readCount = +1
      , kafkaProducerCfg
      , cacConfig
      }

let KaalChakraJobType = < Daily | Weekly | Monthly | Quaterly >

let jobInfoMapx = [ { mapKey = KaalChakraJobType.Daily, mapValue = True } ]

let cacheConfig = { configsExpTime = +86400 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

in  { esqDBReplicaCfg
    , schedulerConfig
    , clickhouseCfg = driverClickhouseCfg
    , kvConfigUpdateFrequency = +10
    , loggerConfigApp =
            common.loggerConfig
        //  { logRawSql = True
            , logFilePath = "/tmp/kaal-chakra.log"
            , prettyPrinting = True
            , level = common.LogLevel.ERROR
            }
    , migrationPath = [] : List Text
    , autoMigrate = False
    , jobInfoMapx
    , cacheConfig
    , httpClientOptions = common.httpClientOptions
    , encTools
    , maxShards = +5
    }