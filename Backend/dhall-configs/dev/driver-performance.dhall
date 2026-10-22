let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

let driverStateRedisCfg =
      base.hedisCfg // { connectDatabase = +2, connectReadOnly = False }

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
          { topicNames =
            [ "adob-sessionizer-searchrequestfordriver"
            , "adob-sessionizer-ride"
            , "adob-sessionizer-booking"
            , "adob-sessionizer-bookingcancellationreason"
            , "adob-sessionizer-fareparameters"
            , "adob-sessionizer-rating"
            ]
          , consumerProperties =
                  base.kafkaConsumerCfg.consumerProperties
              //  { groupId = "driver-performance" }
          }
        , driverStateRedisCfg = Some driverStateRedisCfg
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9987)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath = "/tmp/kafka-consumers-driver-performance.log"
                , logRawSql = False
                }
        }
