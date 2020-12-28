let sec = ./secrets/common.dhall
let globalCommon = ../generic/common.dhall

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

let credRegistry =
  [
    globalCommon.mkCredential "mobility-app-key" "mobility-app" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "fmd-test-app-key" "fmd-test-app" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mobility-bpp-1-key" "JUSPAY.MOBILITY.PROVIDER.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mock-bap-1-key" "JUSPAY.BAP.MOCK.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "juspay-mock-bpp-1-key" "JUSPAY.BPP.MOCK.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , globalCommon.mkCredential "nsdl_bg_1" "NSDL.BG.1" "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="
  ]

in { defaultPoolConfig = globalCommon.defaultPoolConfig
   , smsSessionConfig = globalCommon.smsSessionConfig
   , autoMigrate = globalCommon.autoMigrate
   , loggerConfig = globalCommon.loggerConfig
   , TraceFlag = globalCommon.TraceFlag
   , LogLevel = globalCommon.LogLevel
   , ExotelCfg = globalCommon.ExotelCfg
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
   }
