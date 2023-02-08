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
      { groupId = "groupId", brockers = [ "localhost:29092" ] }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates" ], consumerProperties }

let availabilityTimeWindowOption =
      { period = +7, periodType = common.periodType.Days }

in  { hedisCfg
    , esqDBCfg
    , dumpEvery = +10
    , kafkaConsumerCfg
    , timeBetweenUpdates = +10
    , availabilityTimeWindowOption
    , granualityPeriodType = common.periodType.Hours
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers.log", logRawSql = False }
    }
