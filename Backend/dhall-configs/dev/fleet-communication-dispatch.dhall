let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
                base.kafkaConsumerCfg
            //  { topicNames = [ "fleet-communication-dispatch" ] }
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9995)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath =
                    "/tmp/kafka-consumers-fleet-communication-dispatch.log"
                , logRawSql = True
                }
        }
