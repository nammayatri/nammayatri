let sec = ./secrets/common.dhall

let TraceFlag = < TRACE_INCOMING | TRACE_OUTGOING | TRACE_ALL | TRACE_NOTHING >

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let ExotelCfg = 
  { apiKey : Text
  , apiToken : Text
  , sid : Text
  , callerId : Text
  }

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
   , TraceFlag = TraceFlag
   , LogLevel = LogLevel
   , ExotelCfg = ExotelCfg
   }
