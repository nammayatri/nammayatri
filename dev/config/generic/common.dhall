let sec = ./secrets/common.dhall

let TraceFlag = < TRACE_INCOMING | TRACE_OUTGOING | TRACE_ALL | TRACE_NOTHING >

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let ExotelCfg = 
  { apiKey : Text
  , apiToken : Text
  , sid : Text
  , callerId : Text
  }

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
   , passetto = { _1 = "localhost", _2 = 8021 }
   , fcmJsonPath = None Text
   , autoMigrate = False
   , loggerConfig = loggerConfig
   , TraceFlag = TraceFlag
   , LogLevel = LogLevel
   , ExotelCfg = ExotelCfg
   }
