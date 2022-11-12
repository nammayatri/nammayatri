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
    , smsUserName = sec.smsUserName
    , smsPassword = sec.smsPassword
    , InfoBIPConfig = sec.InfoBIPConfig
    , branchName
    , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
    , fcmJsonPath = Some "/var/local/beckn/jp-beckn-dev-4fbd238801a3.json"
    , googleMapsUrl = "https://maps.googleapis.com/maps/api/"
    , googleMapsKey = sec.googleMapsKey
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , fcmUrl =
        "https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/"
    , registryUrl = "https://api.sandbox.beckn.juspay.in/dev/registry"
    , authServiceUrl = "http://beckn-app-backend-master.atlas:8013"
    }
