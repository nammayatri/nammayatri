let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

in      base
    //  { transport = common.transportKind.RedisStream
        , kafkaConsumerCfg =
            base.kafkaConsumerCfg // { topicNames = [ "ride-events" ] }
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9996)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath = "/tmp/kafka-consumers-ride-events.log"
                , logRawSql = True
                }
        }
