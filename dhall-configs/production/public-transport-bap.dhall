let common = ./common.dhall
let sec = ./secrets/public-transport-bap.dhall

let esqDBCfg =
  { connectHost = "localhost"  --FIXME
  , connectPort = 5438
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_public_transport"
  , connectSchemaName = "atlas_public_transport"
  }

let rcfg =
  { connectHost = "localhost"  --FIXME
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let kafkaProducerCfg =
  { brokers = ["localhost:29092"]  --FIXME
  }

in
{ esqDBCfg = esqDBCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, redisCfg = rcfg
, port = +8023
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/public-transport-bap.log"}
, graceTerminationPeriod = +90
, selfId = "JUSPAY.PUBLIC_TRANSPORT.APP.UAT.1"  --FIXME
, selfURI = "http://localhost:8023/beckn"  --FIXME
, authServiceUrl = common.authServiceUrl
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "juspay-mobility-bap-1-key"  --FIXME
  , signatureExpiry = common.signatureExpiry
  }
, disableSignatureAuth = False
, metricsSearchDurationTimeout = +45
, hostName = "localhost"  --FIXME
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, kafkaProducerCfg = kafkaProducerCfg
}
