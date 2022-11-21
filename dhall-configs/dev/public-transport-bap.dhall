let common = ./common.dhall
let sec = ./secrets/public-transport-bap.dhall

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
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

let kafkaProducerCfg =
  { brokers = ["localhost:29092"]
  }

in
{ esqDBCfg = esqDBCfg
, esqDBReplicaCfg = esqDBReplicaCfg
, migrationPath = Some (env:PUBLIC_TRANSPORT_BAP_MIGRATION_PATH as Text ? "dev/migrations/public-transport-bap")
, autoMigrate = True
, hedisCfg = rcfg
, port = +8023
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/public-transport-bap.log"}
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
, hostName = "localhost"
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, kafkaProducerCfg = kafkaProducerCfg
}
