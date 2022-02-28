let common = ./common.dhall
let sec = ./secrets/public-transport-bap.dhall

let esqDBCfg =
  { connectHost = "beckn-sandbox-v2.cyijte0yeu00.ap-southeast-1.rds.amazonaws.com"
  , connectPort = 5432
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_public_transport_v2"
  , connectSchemaName = "atlas_public_transport"
  }

let rcfg =
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let kafkaProducerCfg =
  { brokers = ["beta-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092"]
  }

in
{ esqDBCfg = esqDBCfg
, migrationPath = None Text
, autoMigrate = common.autoMigrate
, redisCfg = rcfg
, port = +8023
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/public-transport-bap.log"}
, graceTerminationPeriod = +90
, selfId = "api.sandbox.beckn.juspay.in/dev/bap/public-transport/v1"
, selfURI = "https://api.sandbox.beckn.juspay.in/dev/bap/public-transport/v1"
, authServiceUrl = common.authServiceUrl
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "50"  --FIXME
  , signatureExpiry = common.signatureExpiry
  }
, disableSignatureAuth = False
, metricsSearchDurationTimeout = +45
, hostName = "juspay.in"
, httpClientOptions = common.httpClientOptions
, registryUrl = common.registryUrl
, kafkaProducerCfg = kafkaProducerCfg
}
