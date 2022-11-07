let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

let mockRegistryUrl = "http://localhost:8020/"
let nsdlRegistryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/"
let becknOneRegistryUrl = "https://beckn-one.succinct.in/subscribers"
in { smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig // { logToFile = True, logRawSql = True, prettyPrinting = True }
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
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = Some "dummy-fcm.json"
   , googleMapsUrl = "https://maps.googleapis.com/maps/api/"
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "http://localhost:4545/"
   , registryUrl = mockRegistryUrl
   , authServiceUrl = "http://localhost:8013/"
   }
