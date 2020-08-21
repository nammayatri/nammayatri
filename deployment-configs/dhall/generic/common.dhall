let sec = ./secrets/common.dhall

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

let smsConfig =
  { sessionConfig = smsSessionConfig
  , credConfig = {
      username = sec.smsUserName
    , password = sec.smsPassword
    , otpHash = sec.smsOtpHash
    }
  , useFakeSms = Some 7891
  }

{-
let exotelCfg : ExotelCfg =
  { apiKey = ""
  , apiToken = ""
  , sid = ""
  , callerId = ""
  }
-}

in { defaultPoolConfig = defaultPoolConfig
   , smsConfig = smsConfig
   -- , exotelCfg
   , passetto = { _1 = "passetto-hs.atlas", _2 = 8012 }
   , fcmJsonPath = Some "/var/local/beckn/jp-beckn-dev-4fbd238801a3.json"
   , branchName = branchName
   , autoMigrate = False
   }
