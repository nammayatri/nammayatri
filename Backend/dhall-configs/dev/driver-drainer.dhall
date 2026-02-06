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
      , connectionPoolCount = +10
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
      , connectReadOnly = True
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      , connectReadOnly = True
      }

let kafkaProducerCfg =
      { brokers = [ "localhost:29092" ]
      , kafkaCompression = common.kafkaCompression.LZ4
      }

let secondaryKafkaProducerCfg = Some kafkaProducerCfg

let kvConfigUpdateFrequency = +60

let dontEnableForDb = [] : List Text

let dontEnableForKafka = [] : List Text

let kafkaProperties =
        [ { propName = "queue.buffering.max.messages", propValue = "5000" }
        , { propName = "message.max.bytes", propValue = "1000000" }
        ]
      : List { propName : Text, propValue : Text }

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
    , secondaryKafkaProducerCfg
    , kvConfigUpdateFrequency
    , dontEnableForDb
    , dontEnableForKafka
    , kafkaProperties
    }
