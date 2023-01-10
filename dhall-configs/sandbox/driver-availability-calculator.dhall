let common = ./common.dhall

let sec = ./secrets/driver-offer-bpp.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_driver_offer_bpp"
      , connectSchemaName = "atlas_driver_offer_bpp"
      }

let hedisCfg =
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +2
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let consumerProperties =
      { groupId = "driver-availability-compute"
      , brockers = [ "kafka.kafka.svc.cluster.local:9092" ]
      }

let kafkaConsumerCfg =
      { topicNames = [ "location-updates-sandbox" ], consumerProperties }

in  { hedisCfg
    , esqDBCfg
    , dumpEvery = +30
    , kafkaConsumerCfg
    , timeBetweenUpdates = +60
    , granualityPeriodType = common.periodType.Hours
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/kafka-consumers.log", logRawSql = False }
    }
