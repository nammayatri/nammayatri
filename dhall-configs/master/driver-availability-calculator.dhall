let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_driver_offer_bpp_v2"
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
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let consumerProperties =
      { groupId = "driver-availability-compute"
      , brockers = [ "kafka.kafka.svc.cluster.local:9092" ]
      , autoCommit = None Integer
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates-master" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

let cacheConfig = { configsExpTime = +86400 }

in  { hedisCfg
    , esqDBCfg
    , esqDBReplicaCfg
    , cacheConfig
    , dumpEvery = +30
    , kafkaConsumerCfg
    , availabilityTimeWindowOption
    , timeBetweenUpdates = +60
    , granualityPeriodType = common.periodType.Hours
    , httpClientOptions = common.httpClientOptions
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers.log", logRawSql = False }
    }
