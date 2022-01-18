let mkShard =
 \(shardId : Integer) -> \(shortOrgId : Text) ->
  { mapKey = shardId
  , mapValue = shortOrgId
  }

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
  , logToFile = True
  , logToConsole = True
  , logRawSql = True
  , prettyPrinting = False
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

in { defaultPoolConfig = defaultPoolConfig
   , smsSessionConfig = smsSessionConfig
   , autoMigrate = False
   , loggerConfig = loggerConfig
   , LogLevel = LogLevel
   , ExotelCfg = ExotelCfg
   , signatureExpiry = +600 -- in seconds
   , mkShard = mkShard
   , httpClientOptions = httpClientOptions
   }
