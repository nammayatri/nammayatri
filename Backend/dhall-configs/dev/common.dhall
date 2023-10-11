let sec = ./secrets/common.dhall

let globalCommon = ../generic/common.dhall

let mockRegistryUrl = "http://localhost:8020/"

let nsdlRegistryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/"

let becknOneRegistryUrl = "https://beckn-one.succinct.in/subscribers"

let googleCfg =
      { googleMapsUrl = "https://maps.googleapis.com/maps/api/"
      , googleRoadsUrl = "https://roads.googleapis.com/"
      , googleKey = sec.googleKey
      }

let mockGoogleCfg =
      { googleMapsUrl = "http://localhost:8019/"
      , googleRoadsUrl = "http://localhost:8019/"
      , googleKey = "mock-google-key"
      }

let SchedulerType = < RedisBased | DbBased >

in  { smsSessionConfig = globalCommon.smsSessionConfig
    , autoMigrate = globalCommon.autoMigrate
    , loggerConfig =
            globalCommon.loggerConfig
        //  { logToFile = True, logRawSql = True, prettyPrinting = True }
    , LogLevel = globalCommon.LogLevel
    , kafkaCompression = globalCommon.kafkaCompression
    , s3Config = sec.s3Config
    , s3PublicConfig = sec.s3PublicConfig
    , slackToken = sec.slackToken
    , signatureExpiry = globalCommon.signatureExpiry
    , httpClientOptions = globalCommon.httpClientOptions
    , shortDurationRetryCfg = globalCommon.shortDurationRetryCfg
    , longDurationRetryCfg = globalCommon.longDurationRetryCfg
    , ServerName = globalCommon.ServerName
    , periodType = globalCommon.periodType
    , smsUserName = sec.smsUserName
    , smsPassword = sec.smsPassword
    , InfoBIPConfig = sec.InfoBIPConfig
    , passetto = { _1 = "localhost", _2 = 8021 }
    , googleCfg
    , mockGoogleCfg
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , registryUrl = mockRegistryUrl
    , authServiceUrl = "http://localhost:8013/"
    , consumerType = globalCommon.consumerType
    , schedulerType = SchedulerType
    , kafkaOffsetResetConfig = globalCommon.kafkaOffsetResetConfig
    , S3ConfigType = globalCommon.S3Config
    }
