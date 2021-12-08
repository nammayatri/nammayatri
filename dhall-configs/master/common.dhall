let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

let credRegistry =
  [
  -- Gateway
    globalCommon.mkCredential "22" "api.sandbox.beckn.juspay.in/latest/gateway/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "21" "nsdl.co.in"                                 "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="

  -- FMD
  -- BAP
  , globalCommon.mkCredential "juspay-mock-fmd-bap-key-dev"  "JUSPAY.MOCK.FMD.BAP.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "beckn-fmd-bap-key-dev"        "BECKN.FMD.BAP.DEV"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "shopx-fmd-bap-key-dev"        "SHOPX.FMD.BAP.DEV"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-fmd-bap-key-dev"        "PEPPO.FMD.BAP.DEV"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-local-fmd-bap-key-dev"  "PEPPO.LOCAL.FMD.BAP.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-dev-fmd-bap-key-dev"    "PEPPO.DEV.FMD.BAP.DEV"   "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-local-fmd-bap-key-dev" "JUSPAY.FMD.BAP.DEV"      "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "beckn-fmd-bpp-key-dev"                "BECKN.FMD.BPP.DEV"                "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mock-fmd-bpp-key-dev"          "JUSPAY.MOCK.FMD.BPP.DEV"          "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-dunzo-fmd-bpp-key-dev"         "JUSPAY.DUNZO.FMD.BPP.DEV"         "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "lightening-logistics-fmd-bpp-key-dev" "LIGHTENING.LOGISTICS.FMD.BPP.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="

  -- Mobility
  -- BAP
  , globalCommon.mkCredential "19" "api.sandbox.beckn.juspay.in/latest/bap/cab/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "12" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/565db72a-04d4-4211-90ae-c956461397b2" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "13" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/a45f243f-9915-4842-b78b-6d718844a48d" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "14" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/fb6ee235-8cf5-4f8f-aba2-40de1fa733d1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "15" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/c26001a5-8a20-4e77-bebd-f9d7fce618bc" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "16" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/384786e9-63e1-4f00-bbd9-40480387907d" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "17" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/dc46e80a-99d7-4f96-9949-2c045106b081" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "18" "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/092ef105-6fe6-4eab-9c6f-e8a57b51e1af" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="

  -- Metro
  -- BAP
  , globalCommon.mkCredential "20" "api.sandbox.beckn.juspay.in/latest/bap/metro/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "25" "api.sandbox.beckn.juspay.in/bpp/metro/v1"     "OGfSqt352PXRfdd+pLXo3eLLd96iL8dcbireMynl5A4="

  -- Parking
  -- BAP
  , globalCommon.mkCredential "56" "api.sandbox.beckn.juspay.in/dev/bap/parking/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  ]

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
   , branchName = branchName
   , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
   , fcmJsonPath = Some "/var/local/beckn/jp-beckn-dev-4fbd238801a3.json"
   , credRegistry = credRegistry
   , signingKeys = sec.signingKeys
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/"
   , graphhopperUrl = "https://api.sandbox.beckn.juspay.in/map/grphr/"
   , registryUrl = "https://api.sandbox.beckn.juspay.in/latest/registry"
   , authServiceUrl = "http://beckn-app-backend-release.atlas:8013"
   }
