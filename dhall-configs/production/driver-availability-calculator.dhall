let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let esqDBCfg =
      { connectHost = "adb.driver.primary.beckn.juspay.net"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_driver_offer_bpp"
      , connectSchemaName = "atlas_driver_offer_bpp"
      }

let esqDBReplicaCfg =
      { connectHost = "adb.driver.reporting.beckn.juspay.net"
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let hedisCfg =
      { connectHost = "cache.primary.beckn.juspay.net"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let consumerProperties =
      { groupId = "driver-availability-compute"
      , brockers = [ "atlas-c2-kafka-brokers.kafka-cluster:9092" ]
      , autoCommit = None Integer
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates-production" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

in  { hedisCfg
    , esqDBCfg
    , esqDBReplicaCfg
    , cacheConfig
    , dumpEvery = +120
    , kafkaConsumerCfg
    , timeBetweenUpdates = +60
    , availabilityTimeWindowOption
    , granualityPeriodType = common.periodType.Hours
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/driver-availability-calculator.log"
            , logRawSql = False
            }
    }
