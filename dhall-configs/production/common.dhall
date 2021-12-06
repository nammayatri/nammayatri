let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

let credRegistry =
  [
  -- Gateway
    globalCommon.mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "nsdl_bg_1"       "NSDL.BG.1"   "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="

  -- FMD
  -- BAP
  , globalCommon.mkCredential "beckn-fmd-bap-key-prod"        "BECKN.FMD.BAP.PROD"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "shopx-fmd-bap-key-prod"        "SHOPX.FMD.BAP.PROD"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-fmd-bap-key-prod"        "PEPPO.FMD.BAP.PROD"       "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-local-fmd-bap-key-prod"  "PEPPO.LOCAL.FMD.BAP.PROD" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "peppo-dev-fmd-bap-key-prod"    "PEPPO.DEV.FMD.BAP.PROD"   "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-local-fmd-bap-key-prod" "JUSPAY.FMD.BAP.PROD"      "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "juspay-dunzo-fmd-bpp-key-prod"         "JUSPAY.DUNZO.FMD.BPP.PROD"         "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "lightening-logistics-fmd-bpp-key-prod" "LIGHTENING.LOGISTICS.FMD.BPP.PROD" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="

  -- Mobility
  -- BAP
  , globalCommon.mkCredential "juspay-mobility-bap-1-key-prod" "JUSPAY.MOBILITY.APP.1.PROD" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "juspay-mobility-bpp-1-key-prod"  "JUSPAY.MOBILITY.PROVIDER.1.PROD"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
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
   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , branchName = branchName
   , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
   , fcmJsonPath = Some "/var/local/beckn/beckn-fcm.json"
   , credRegistry = credRegistry
   , signingKeys = sec.signingKeys
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "https://fcm.googleapis.com/v1/projects/beckn-d4a42/messages:send/"
   , graphhopperUrl = "http://graphhopper.atlas/"
   , registryUrl = "https://gateway-1.beckn.nsdl.co.in"
   , authServiceUrl = "https://api.beckn.juspay.in/app/"
   }
