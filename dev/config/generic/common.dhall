let sec = ./secrets/common.dhall

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
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = None Text
   , autoMigrate = False
   }
