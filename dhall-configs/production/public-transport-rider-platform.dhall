let common = ./common.dhall

let sec = ./secrets/public-transport-rider-platform.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5438
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_public_transport"
      , connectSchemaName = "atlas_public_transport"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5435
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
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

let kafkaProducerCfg = { brokers = [] : List Text }

in  { esqDBCfg
    , esqDBReplicaCfg
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , redisCfg = rcfg
    , port = +8023
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/public-transport-rider-platform.log" }
    , graceTerminationPeriod = +90
    , selfId = "JUSPAY.PUBLIC_TRANSPORT.APP.UAT.1"
    , selfURI = "http://localhost:8023/beckn"
    , authServiceUrl = common.authServiceUrl
    , authEntity =
      { signingKey = sec.signingKey
      , uniqueKeyId = "juspay-mobility-bap-1-key"
      , signatureExpiry = common.signatureExpiry
      }
    , disableSignatureAuth = False
    , metricsSearchDurationTimeout = +45
    , hostName = "localhost"
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , registryUrl = common.registryUrl
    , kafkaProducerCfg
    }
