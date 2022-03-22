let common = ./common.dhall

let hcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

let kafkaConsumerCfgs =
  { publicTransportQuotes = { brokers = ["localhost:29092"],
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
