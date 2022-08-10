let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig
   , LogLevel = globalCommon.LogLevel
   , ExotelCfg = globalCommon.ExotelCfg
   , exotelCfg = sec.exotelCfg
   , s3Config = sec.s3Config
   , signatureExpiry = globalCommon.signatureExpiry
   , httpClientOptions = globalCommon.httpClientOptions
   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , branchName = branchName
   , passetto = { _1 = "passetto-hs.passetto.svc.cluster.local", _2 = 8012 }
   , fcmJsonPath = Some "/var/local/beckn/beckn-fcm.json"
   , googleMapsUrl = "https://maps.googleapis.com/maps/api/"
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "https://fcm.googleapis.com/v1/projects/beckn-d4a42/messages:send/"
   , graphhopperUrl = "http://graphhopper.atlas.svc.cluster.local:8989/"
   , registryUrl = "https://gateway-1.beckn.nsdl.co.in"
   , authServiceUrl = "http://beckn-app-backend-production.atlas:8013"
   }
