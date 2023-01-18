let sec = ./secrets/common.dhall

let globalCommon = ../generic/common.dhall

let branchName = "\$DEPLOY_VARIANT"

in  { smsSessionConfig = globalCommon.smsSessionConfig
    , autoMigrate = globalCommon.autoMigrate
    , loggerConfig = globalCommon.loggerConfig // { logRawSql = True }
    , LogLevel = globalCommon.LogLevel
    , ExotelCfg = globalCommon.ExotelCfg
    , exotelCfg = sec.exotelCfg
    , s3Config = sec.s3Config
    , idfyCfg = sec.idfyCfg
    , slackToken = sec.slackToken
    , signatureExpiry = globalCommon.signatureExpiry
    , httpClientOptions = globalCommon.httpClientOptions
    , shortDurationRetryCfg = globalCommon.shortDurationRetryCfg
    , longDurationRetryCfg = globalCommon.longDurationRetryCfg
    , ServerName = globalCommon.ServerName
    , smsUserName = sec.smsUserName
    , smsPassword = sec.smsPassword
    , InfoBIPConfig = sec.InfoBIPConfig
    , branchName
    , periodType = globalCommon.periodType
    , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , registryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"
    , authServiceUrl = "http://beckn-app-backend-sandbox.atlas:8013"
    , consumerType = globalCommon.consumerType
    }
