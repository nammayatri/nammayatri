let common = ./common.dhall

let main = ./public-transport-rider-platform.dhall

let juspayGatewayUrl = "http://localhost:8015/v1"

let kafkaConsumerCfgs =
      { publicTransportSearch =
        { brokers = [ "localhost:29092" ]
        , groupId = "publicTransportSearchGroup"
        , timeoutMilliseconds = +10000
        , kafkaCompression = common.kafkaCompression.LZ4
        }
      }

let criticalAPIs = { criticalAPIList = [ "/beckn/:merchantId/search/" ] }

in  { esqDBCfg = main.esqDBCfg
    , migrationPath = main.migrationPath
    , autoMigrate = main.autoMigrate
    , port = +8024
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/public-transport-search-consumer.log" }
    , graceTerminationPeriod = +90
    , bapId = main.selfId
    , bapURI = main.selfURI
    , gatewayUrl = juspayGatewayUrl
    , httpClientOptions = main.httpClientOptions
    , shortDurationRetryCfg = main.shortDurationRetryCfg
    , longDurationRetryCfg = main.longDurationRetryCfg
    , authEntity = main.authEntity
    , kafkaConsumerCfgs
    , criticalAPIs
    }
