let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

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
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
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

let consumerProperties =
      { groupId = "groupId"
      , brockers = [ "localhost:29092" ]
      , autoCommit = None Integer
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

in  { hedisCfg
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
    }
