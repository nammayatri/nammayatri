let mkShard =
      \(shardId : Integer) ->
      \(shortOrgId : Text) ->
        { mapKey = shardId, mapValue = shortOrgId }

let PeriodType = < Minutes | Hours | Days | Months | Years >

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

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
      , logRawSql = False
      , prettyPrinting = False
      }

let ConsumerType = < AVAILABILITY_TIME | BROADCAST_MESSAGE >

let httpClientOptions = { timeoutMs = +2000 }

let shortDurationRetryCfg = { maxRetries = +3, baseCoefficient = +2 }

let longDurationRetryCfg = { maxRetries = +3, baseCoefficient = +4 }

let ServerName = < APP_BACKEND | BECKN_TRANSPORT | DRIVER_OFFER_BPP >

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
    }
