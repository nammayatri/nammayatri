let mkShard =
      \(shardId : Integer) ->
      \(shortOrgId : Text) ->
        { mapKey = shardId, mapValue = shortOrgId }

let PeriodType = < Minutes | Hours | Days | Months | Years >

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let KafkaCompression = < NO_COMPRESSION | GZIP | SNAPPY | LZ4 >

let S3AwsConfig =
      { accessKeyId : Text
      , secretAccessKey : Text
      , bucketName : Text
      , region : Text
      , pathPrefix : Text
      }

let S3MockConfig =
      { baseLocalDirectory : Text, bucketName : Text, pathPrefix : Text }

let S3Config = < S3AwsConf : S3AwsConfig | S3MockConf : S3MockConfig >

let smsSessionConfig = { attempts = +3, authExpiry = +3, tokenExpiry = +365 }

let loggerConfig =
      { level = LogLevel.DEBUG
      , logToFile = False
      , logToConsole = True
      , logRawSql = True
      , prettyPrinting = False
      }

let ConsumerType = < AVAILABILITY_TIME | BROADCAST_MESSAGE | PERSON_STATS >

let kafkaConfig = { topicName : Text, kafkaKey : Text }

let eventStreamNameType =
      < KAFKA_STREAM | LOG_STREAM | PROMETHEUS_STREAM | REDIS_STREAM >

let streamConfig =
      < KafkaStream : kafkaConfig | LogStream : Text | PrometheusStream : Text >

let eventType =
      < RideCreated
      | RideStarted
      | RideEnded
      | RideCancelled
      | BookingCreated
      | BookingCancelled
      | BookingCompleted
      | SearchRequest
      | Quotes
      | Estimate
      >

let httpClientOptions = { timeoutMs = +2000 }

let shortDurationRetryCfg = { maxRetries = +3, baseCoefficient = +2 }

let longDurationRetryCfg = { maxRetries = +3, baseCoefficient = +4 }

let ServerName =
      < APP_BACKEND
      | APP_BACKEND_MANAGEMENT
      | DRIVER_OFFER_BPP
      | DRIVER_OFFER_BPP_MANAGEMENT
      | SPECIAL_ZONE
      >

let SchedulerType = < RedisBased | DbBased >

in  { smsSessionConfig
    , autoMigrate = False
    , loggerConfig
    , LogLevel
    , signatureExpiry = +600
    , mkShard
    , httpClientOptions
    , shortDurationRetryCfg
    , longDurationRetryCfg
    , ServerName
    , S3Config
    , periodType = PeriodType
    , consumerType = ConsumerType
    , kafkaCompression = KafkaCompression
    , kafkaConfig
    , streamConfig
    , eventStreamNameType
    , eventType
    , schedulerType = SchedulerType
    }
