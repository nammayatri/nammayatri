let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

let ijBehaviorTickStream =
      { streamPrefix = "dynamic-offer-driver-app:ij_behavior_tick_stream_"
      , shardCount = +10
      , consumerGroupName = "ij-behavior-tick-consumers"
      , readBatchSize = +50
      , readBlockMilliseconds = +2000
      , claimMinIdleMs = +60000
      , claimIntervalSeconds = +30
      , maxDeliveries = +5
      , pauseFlagKey = "ij_behavior_tick_stream_paused"
      , pauseSleepSeconds = +5
      }

in      base
    //  { transport = common.transportKind.RedisStream
        , kafkaConsumerCfg =
            base.kafkaConsumerCfg // { topicNames = [ "ij-behavior-tick" ] }
        , redisStreamCfg = Some ijBehaviorTickStream
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9997)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath = "/tmp/kafka-consumers-ij-behavior-tick.log"
                , logRawSql = True
                }
        }
