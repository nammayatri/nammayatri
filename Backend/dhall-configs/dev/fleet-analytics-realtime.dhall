let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
                base.kafkaConsumerCfg
            //  { topicNames = [ "fleet-analytics-realtime" ] }
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9992)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath =
                    "/tmp/kafka-consumers-fleet-analytics-realtime.log"
                , logRawSql = True
                }
        }
