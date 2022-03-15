let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig // { logRawSql = True }
   , LogLevel = globalCommon.LogLevel
   , ExotelCfg = globalCommon.ExotelCfg
   , exotelCfg = sec.exotelCfg
   , signatureExpiry = globalCommon.signatureExpiry
   , httpClientOptions = globalCommon.httpClientOptions
   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , branchName = branchName
   , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
   , fcmJsonPath = Some "/var/local/beckn/jp-beckn-dev-4fbd238801a3.json"
   , googleMapsUrl = "https://maps.googleapis.com/maps/api/"
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/"
   , graphhopperUrl = "https://api.sandbox.beckn.juspay.in/map/grphr/"
   , registryUrl = "https://api.sandbox.beckn.juspay.in/dev/registry"
   , authServiceUrl = "http://beckn-app-backend-master.atlas:8013"
   }
