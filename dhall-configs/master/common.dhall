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
  , globalCommon.mkCredential "juspay-mobility-bap-1-key-dev" "JUSPAY.MOBILITY.APP.1.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  -- BPP
  , globalCommon.mkCredential "juspay-mobility-bpp-1-key-dev" "JUSPAY.MOBILITY.PROVIDER.1.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-2-key-dev" "JUSPAY.MOBILITY.PROVIDER.2.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-3-key-dev" "JUSPAY.MOBILITY.PROVIDER.3.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-4-key-dev" "JUSPAY.MOBILITY.PROVIDER.4.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-5-key-dev" "JUSPAY.MOBILITY.PROVIDER.5.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-6-key-dev" "JUSPAY.MOBILITY.PROVIDER.6.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-7-key-dev" "JUSPAY.MOBILITY.PROVIDER.7.DEV" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
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
   }
