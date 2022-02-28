let common = ./common.dhall


let hcfg =
  { connectHost = "ec-redis-beta.bfw4iw.ng.0001.apse1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +2
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let kafkaConsumerCfgs =
  { publicTransportQuotes = {brokers = ["beta-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092"], groupId = "publicTransportQuotesGroup"}
  }

in
{ port = +8024
, graceTerminationPeriod = +90
, hedisCfg = hcfg
, kafkaConsumerCfgs = kafkaConsumerCfgs
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/search-result-aggregator.log"}
}
