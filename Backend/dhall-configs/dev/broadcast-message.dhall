let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
            base.kafkaConsumerCfg // { topicNames = [ "broadcast-messages" ] }
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9994)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath = "/tmp/kafka-consumers-broadcast-messages.log"
                , logRawSql = True
                }
        }
