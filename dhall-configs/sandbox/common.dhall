let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

let credRegistry =
  [
  -- Gateway
    globalCommon.mkCredential "39" "api.sandbox.beckn.juspay.in/gateway/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "21" "nsdl.co.in"                                 "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="

  -- FMD
  -- BAP
  , globalCommon.mkCredential "juspay-mock-fmd-bap-key-sandbox"  "JUSPAY.MOCK.FMD.BAP.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "beckn-fmd-bap-key-sandbox"        "BECKN.FMD.BAP.SANDBOX"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "shopx-fmd-bap-key-sandbox"        "SHOPX.FMD.BAP.SANDBOX"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-fmd-bap-key-sandbox"        "PEPPO.FMD.BAP.SANDBOX"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-local-fmd-bap-key-sandbox"  "PEPPO.LOCAL.FMD.BAP.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-dev-fmd-bap-key-sandbox"    "PEPPO.DEV.FMD.BAP.SANDBOX"   "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-local-fmd-bap-key-sandbox" "JUSPAY.FMD.BAP.SANDBOX"      "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "beckn-fmd-bpp-key-sandbox"                "BECKN.FMD.BPP.SANDBOX"                "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mock-fmd-bpp-key-sandbox"          "JUSPAY.MOCK.FMD.BPP.SANDBOX"          "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-dunzo-fmd-bpp-key-sandbox"         "JUSPAY.DUNZO.FMD.BPP.SANDBOX"         "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "lightening-logistics-fmd-bpp-key-sandbox" "LIGHTENING.LOGISTICS.FMD.BPP.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="

  -- Mobility
  -- BAP
  , globalCommon.mkCredential "35" "api.sandbox.beckn.juspay.in/bap/cab/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "37" "api.sandbox.beckn.juspay.in/bpp/cab/v1/3041599b-2fcf-45e1-bfd5-115db5cd1353" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "38" "api.sandbox.beckn.juspay.in/bpp/cab/v1/87a04bab-bc3b-4d2a-866a-3c5ee9cc3b34" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "50" "api.sandbox.beckn.juspay.in/bpp/cab/v1/8dfcf59d-e700-4ab1-a72e-12d716ae560c" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "51" "api.sandbox.beckn.juspay.in/bpp/cab/v1/2672dcf8-9c90-4e42-9644-0c3587ee00ac" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "52" "api.sandbox.beckn.juspay.in/bpp/cab/v1/ecddd512-585b-48b3-84d8-b325e09971d0" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "53" "api.sandbox.beckn.juspay.in/bpp/cab/v1/9eb9d5bd-1cf4-49a2-87db-df3d1a2c632d" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "54" "api.sandbox.beckn.juspay.in/bpp/cab/v1/110f9ce2-3618-46b1-aa3a-2ecb819a3088" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "55" "api.sandbox.beckn.juspay.in/bpp/cab/v1/07c66658-4d96-4181-b302-ad69de156156" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "56" "api.sandbox.beckn.juspay.in/bpp/cab/v1/76bbaf29-b245-46c4-aa2d-b57aca41b3da" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="

  -- Metro
  -- BAP
  , globalCommon.mkCredential "36" "api.sandbox.beckn.juspay.in/bap/metro/v1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "25" "api.sandbox.beckn.juspay.in/bpp/metro/v1"     "OGfSqt352PXRfdd+pLXo3eLLd96iL8dcbireMynl5A4="
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
   , registryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in"
   , authServiceUrl = "https://api.sandbox.beckn.juspay.in/dev/bap/"
   }
