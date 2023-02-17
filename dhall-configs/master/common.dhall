let sec = ./secrets/common.dhall

let globalCommon = ../generic/common.dhall

let branchName = "master"

in  { smsSessionConfig = globalCommon.smsSessionConfig
    , autoMigrate = globalCommon.autoMigrate
    , loggerConfig = globalCommon.loggerConfig // { logRawSql = True }
    , LogLevel = globalCommon.LogLevel
    , ExotelCfg = globalCommon.ExotelCfg
    , ServerName = globalCommon.ServerName
    , exotelCfg = sec.exotelCfg
    , s3Config = sec.s3Config
    , idfyCfg = sec.idfyCfg
    , slackToken = sec.slackToken
    , signatureExpiry = globalCommon.signatureExpiry
    , httpClientOptions = globalCommon.httpClientOptions
    , shortDurationRetryCfg = globalCommon.shortDurationRetryCfg
    , longDurationRetryCfg = globalCommon.longDurationRetryCfg
    , smsUserName = sec.smsUserName
    , smsPassword = sec.smsPassword
    , InfoBIPConfig = sec.InfoBIPConfig
    , branchName
    , periodType = globalCommon.periodType
    , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , registryUrl = "https://api.sandbox.beckn.juspay.in/dev/registry"
    , authServiceUrl = "http://beckn-app-backend-master.atlas:8013"
    , consumerType = globalCommon.consumerType
    , snapToRoadSnippetThreshold = globalCommon.snapToRoadSnippetThreshold
    }
