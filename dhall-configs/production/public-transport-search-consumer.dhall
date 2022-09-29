let common = ./common.dhall
let main = ./public-transport-bap.dhall

let juspayGatewayUrl = "http://localhost:8015/v1"  --FIXME

let kafkaConsumerCfgs =
  { publicTransportSearch = {brokers = ["localhost:29092"], groupId = "publicTransportSearchGroup"}  --FIXME
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
, authEntity = main.authEntity
, kafkaConsumerCfgs = kafkaConsumerCfgs
}
