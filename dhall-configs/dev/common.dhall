let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

let mockRegistryUrl = "http://localhost:8020/"
let nsdlRegistryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/"
let becknOneRegistryUrl = "https://beckn-one.succinct.in/subscribers"
let loggerConfig =
  { level = globalCommon.LogLevel.DEBUG
  , logToFile = True
  , logToConsole = True
  , logRawSql = True
  , prettyPrinting = True
  }


in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = loggerConfig
   , LogLevel = globalCommon.LogLevel
   , ExotelCfg = globalCommon.ExotelCfg
   , exotelCfg = sec.exotelCfg
   , signatureExpiry = globalCommon.signatureExpiry
   , httpClientOptions = globalCommon.httpClientOptions

   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = Some "dummy-fcm.json"
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "http://localhost:4545/"
   , graphhopperUrl = "https://api.sandbox.beckn.juspay.in/map/grphr/"
   , registryUrl = mockRegistryUrl
   , authServiceUrl = "http://localhost:8013/"
   }
