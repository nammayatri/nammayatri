let common = ./common.dhall
let main = ./public-transport-bap.dhall

let juspayGatewayUrl = "https://api.sandbox.beckn.juspay.in/dev/gateway/v1/"

let kafkaConsumerCfgs =
  { publicTransportSearch = {brokers = ["beta-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092"], groupId = "publicTransportSearchGroup"}
  }

in
{ esqDBCfg = main.esqDBCfg
, migrationPath = main.migrationPath
, autoMigrate = main.autoMigrate
, port = +8024
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/public-transport-search-consumer.log"}
, graceTerminationPeriod = +90
, bapId = main.selfId
, bapURI = main.selfURI
, gatewayUrl = juspayGatewayUrl
, httpClientOptions = main.httpClientOptions
, authServiceUrl = main.authServiceUrl
, authEntity = main.authEntity
, kafkaConsumerCfgs = kafkaConsumerCfgs
}
