let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

let credRegistry =
  [
    globalCommon.mkCredential "mobility-app-key" "mobility-app" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "fmd-test-app-key" "fmd-test-app" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-fmd-1-key" "JUSPAY.FMD.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.2" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.3" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-1-key" "JUSPAY.MOBILITY.PROVIDER.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mock-bap-1-key" "JUSPAY.BAP.MOCK.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mock-bpp-1-key" "JUSPAY.BPP.MOCK.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "another-test-cabs" "another-test-cabs" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "nsdl_bg_1" "NSDL.BG.1" "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="
  , globalCommon.mkCredential "metro-bpp-key" "metro-bpp" "OGfSqt352PXRfdd+pLXo3eLLd96iL8dcbireMynl5A4="
  ]

let mockRegistryUrl = "http://localhost:8020/"
let nsdlRegistryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/"
let becknOneRegistryUrl = "https://beckn-one.succinct.in/subscribers"

in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig
   , LogLevel = globalCommon.LogLevel
   , ExotelCfg = globalCommon.ExotelCfg
   , exotelCfg = sec.exotelCfg
   , signatureExpiry = globalCommon.signatureExpiry
   , mkCredential = globalCommon.mkCredential
   , mkSigningKey = globalCommon.mkSigningKey
   , httpClientOptions = globalCommon.httpClientOptions

   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = Some "dummy-fcm.json"
   , credRegistry = credRegistry
   , signingKeys = sec.signingKeys
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "http://localhost:4545/"
   , graphhopperUrl = "https://api.sandbox.beckn.juspay.in/map/grphr/"
   , registryUrl = mockRegistryUrl
   , authServiceUrl = "http://localhost:8013/"
   }
