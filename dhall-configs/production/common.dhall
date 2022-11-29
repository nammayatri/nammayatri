let sec = ./secrets/common.dhall

let globalCommon = ../generic/common.dhall

let branchName = "production"

in  { smsSessionConfig = globalCommon.smsSessionConfig
    , autoMigrate = globalCommon.autoMigrate
    , loggerConfig = globalCommon.loggerConfig
    , LogLevel = globalCommon.LogLevel
    , ExotelCfg = globalCommon.ExotelCfg
    , exotelCfg = sec.exotelCfg
    , s3Config = sec.s3Config
    , idfyCfg = sec.idfyCfg
    , slackToken = sec.slackToken
    , signatureExpiry = globalCommon.signatureExpiry
    , httpClientOptions = globalCommon.httpClientOptions
    , ServerName = globalCommon.ServerName
    , smsUserName = sec.smsUserName
    , smsPassword = sec.smsPassword
    , InfoBIPConfig = sec.InfoBIPConfig
    , branchName
    , periodType = globalCommon.periodType
    , passetto = { _1 = "passetto-hs.passetto.svc.cluster.local", _2 = 8012 }
    , fcmJsonPath = Some "/var/local/beckn/beckn-fcm.json"
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , fcmUrl =
        "https://fcm.googleapis.com/v1/projects/namma-yatri/messages:send/"
    , registryUrl = "https://api.beckn.juspay.in/registry"
    , authServiceUrl = "http://beckn-app-backend-production.atlas:8013"
    }
