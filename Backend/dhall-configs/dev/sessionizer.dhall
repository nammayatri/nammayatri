let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

let driverStateRedisCfg =
      base.hedisCfg // { connectDatabase = +2, connectReadOnly = False }

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
          { topicNames =
            [ "location-updates", "dynamic-offer-driver-events-updates" ]
          , consumerProperties =
                  base.kafkaConsumerCfg.consumerProperties
              //  { groupId = "sessionizer" }
          }
        , driverStateRedisCfg = Some driverStateRedisCfg
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9988)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath = "/tmp/kafka-consumers-sessionizer.log"
                , logRawSql = False
                }
        }
