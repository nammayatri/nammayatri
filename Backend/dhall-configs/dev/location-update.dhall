let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

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

let consumerProperties =
      { groupId = "groupId"
      , brockers = [ "localhost:29092" ]
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
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_kafka"
      , tls = False
      , retryInterval = [ +0 ]
      }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let serviceClickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "localhost"
      , port = 8123
      , password = sec.clickHousePassword
      , database = "atlas_app"
      , tls = False
      , retryInterval = [ +0 ]
      }

let dashboardClickhouseCfg = serviceClickhouseCfg

let cacConfig =
      { host = "http://localhost:8080"
      , interval = 10
      , tenant = "test"
      , retryConnection = False
      , cacExpTime = +86400
      , enablePolling = True
      , enableCac = False
      }

let inMemConfig = { enableInMem = True, maxInMemSize = +100000000 }

in  { hedisCfg
    , hedisClusterCfg
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
    , metricsPort = +9994
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
    , kafkaReadBatchDelay = +10
    , kafkaReadBatchSize = +10
    , consumerStartTime = Some +14
    , consumerEndTime = Some +20
    , dashboardClickhouseCfg
    , inMemConfig
    }
