let common = ./common.dhall

let base = ./kafka-consumers-base.dhall

let rideEventsStream =
      { streamPrefix = "dynamic-offer-driver-app:ride_events_stream_"
      , shardCount = +10
      , consumerGroupName = "ride-events-consumers"
      , readBatchSize = +50
      , -- Must stay comfortably under hedis' cluster node request timeout
        -- (REDIS_REQUEST_NODE_TIMEOUT, default 5s). At 5000 the BLOCK deadline races
        -- that timeout and every idle read loses, logging a TimeoutException per shard
        -- per read — that alone grew this consumer's log past 50GB.
        readBlockMilliseconds = +2000
      , claimMinIdleMs = +60000
      , claimIntervalSeconds = +30
      , maxDeliveries = +5
      , pauseFlagKey = "ride_events_stream_paused"
      , pauseSleepSeconds = +5
      }

in      base
    //  { transport = common.transportKind.RedisStream
        , kafkaConsumerCfg =
            base.kafkaConsumerCfg // { topicNames = [ "ride-events" ] }
        , redisStreamCfg = Some rideEventsStream
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9995)
        , loggerConfig =
                base.loggerConfig
            //  { logFilePath = "/tmp/kafka-consumers-ride-events.log"
                , logRawSql = True
                }
        }
