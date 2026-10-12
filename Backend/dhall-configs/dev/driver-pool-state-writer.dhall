let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

let driverStateRedisCfg =
      base.hedisCfg // { connectDatabase = +2, connectReadOnly = False }

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
          { topicNames = [ "driver-pool-state" ]
          , consumerProperties =
                  base.kafkaConsumerCfg.consumerProperties
              //  { groupId = "driver-pool-state-writer" }
          }
        , driverStateRedisCfg = Some driverStateRedisCfg
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9989)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath =
                    "/tmp/kafka-consumers-driver-pool-state-writer.log"
                , logRawSql = False
                }
        }
