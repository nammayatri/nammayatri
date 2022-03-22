let common = ./common.dhall


let hcfg =
  { connectHost = "beckn-redis-001-001.zkt6uh.0001.aps1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

let kafkaConsumerCfgs =
  { publicTransportQuotes = { brokers = ["alpha-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092"],
                              groupId = "publicTransportQuotesGroup",
                              timeoutMilliseconds = +10000 }
  }

in
{ port = +8025
, graceTerminationPeriod = +90
, hedisCfg = hcfg
, kafkaConsumerCfgs = kafkaConsumerCfgs
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/search-result-aggregator.log"}
}
