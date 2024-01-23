let common = ./common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let globalCommon = ../generic/common.dhall

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

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let kvConfigUpdateFrequency = +60

let dontEnableForDb = [] : List Text

let dontEnableForKafka = [] : List Text

let maxMessages
    : Text
    = "5000"

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/dynamic-offer-driver-app.log"
            , logRawSql = True
            }
    , kafkaProducerCfg
    , kvConfigUpdateFrequency
    , dontEnableForDb
    , dontEnableForKafka
    , maxMessages
    }
