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
  , globalCommon.mkCredential "juspay-mobility-bap-1-key-sandbox" "JUSPAY.MOBILITY.APP.1.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "juspay-mobility-bpp-1-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.1.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-2-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.2.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-3-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.3.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-4-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.4.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-5-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.5.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-6-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.6.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-7-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.7.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-8-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.8.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-9-key-sandbox"  "JUSPAY.MOBILITY.PROVIDER.9.SANDBOX"  "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-10-key-sandbox" "JUSPAY.MOBILITY.PROVIDER.10.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-11-key-sandbox" "JUSPAY.MOBILITY.PROVIDER.11.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-12-key-sandbox" "JUSPAY.MOBILITY.PROVIDER.12.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-13-key-sandbox" "JUSPAY.MOBILITY.PROVIDER.13.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-14-key-sandbox" "JUSPAY.MOBILITY.PROVIDER.14.SANDBOX" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  ]

in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig
   , TraceFlag = globalCommon.TraceFlag
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
   , fcmJsonPath = Some "/var/local/beckn/jp-beckn-dev-4fbd238801a3.json"
   , credRegistry = credRegistry
   , signingKeys = sec.signingKeys
   , googleMapsKey = sec.googleMapsKey
   , fcmUrl = "https://fcm.googleapis.com/v1/projects/jp-beckn-dev/messages:send/"
   }
