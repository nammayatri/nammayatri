let common = ./common.dhall

let sec = ./secrets/public-transport-bap.dhall

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_public_transport_v2"
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
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +1
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let kafkaProducerCfg =
      { brokers = [ "alpha-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092" ]
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , hedisCfg = rcfg
    , port = +8023
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/public-transport-bap.log" }
    , graceTerminationPeriod = +90
    , selfId = "api.sandbox.beckn.juspay.in/dev/bap/public-transport/v1"
    , selfURI =
        "https://api.sandbox.beckn.juspay.in/dev/bap/public-transport/v1"
    , authServiceUrl = common.authServiceUrl
    , authEntity =
      { signingKey = sec.signingKey
      , uniqueKeyId = "50"
      , signatureExpiry = common.signatureExpiry
      }
    , disableSignatureAuth = False
    , hostName = "juspay.in"
    , httpClientOptions = common.httpClientOptions
    , registryUrl = common.registryUrl
    , kafkaProducerCfg
    }
