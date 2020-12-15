let sec = ./secrets/common.dhall

let mkCredential =
 \(uniqueKeyId : Text) -> \(shortOrgId : Text) -> \(signPubKey : Text) ->
  {
    uniqueKeyId = uniqueKeyId
  , shortOrgId = shortOrgId
  , signPubKey = signPubKey
  }

let TraceFlag = < TRACE_INCOMING | TRACE_OUTGOING | TRACE_ALL | TRACE_NOTHING >

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let ExotelCfg =
  { apiKey : Text
  , apiToken : Text
  , sid : Text
  , callerId : Text
  }

let credRegistry =
  [
    mkCredential "mobility-app-key" "mobility-app" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "fmd-test-app-key" "fmd-test-app" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "juspay-mobility-bap-1-key" "JUSPAY.MOBILITY.APP.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "juspay-mobility-bpp-1-key" "JUSPAY.MOBILITY.PROVIDER.UAT.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "juspay-bg-1-key" "JUSPAY.BG.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "juspay-mock-bap-1-key" "JUSPAY.BAP.MOCK.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "juspay-mock-bpp-1-key" "JUSPAY.BPP.MOCK.1" "kCa4OlmRVfCPcvzjPPGik0Ljei5dRYuuj/2K6upaf1E="
  , mkCredential "nsdl_bg_1" "NSDL.BG.1" "Fhjwaka1Za+ld+7Nms7S0C675r24mZoyWVn8JbYTjSs="
  ]

-- To be substituted during deployment
let branchName = "$DEPLOY_VARIANT"

let defaultPoolConfig =
    { stripes = +1
    , keepAlive = +10
    , resourcesPerStripe = +50
    }

let smsSessionConfig =
  { attempts = +3
  , authExpiry = +3
  , tokenExpiry = +365
  }

let loggerConfig =
  { level = LogLevel.DEBUG
  , isAsync = True
  , logToFile = True
  , logToConsole = True
  , logRawSql = True
  }

in { defaultPoolConfig = defaultPoolConfig
   , smsUserName = sec.smsUserName
   , smsPassword = sec.smsPassword
   , smsSessionConfig = smsSessionConfig
   , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
   , fcmJsonPath = Some "/var/local/beckn/jp-beckn-dev-4fbd238801a3.json"
   , branchName = branchName
   , autoMigrate = False
   , loggerConfig = loggerConfig
   , credRegistry = credRegistry
   , signingKeys = sec.signingKeys
   , TraceFlag = TraceFlag
   , LogLevel = LogLevel
   , ExotelCfg = ExotelCfg
   , signatureExpiry = +600 -- in seconds
   }
