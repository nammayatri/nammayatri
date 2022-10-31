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

let smsSessionConfig =
  { attempts = +3
  , authExpiry = +3
  , tokenExpiry = +365
  }

let loggerConfig =
  { level = LogLevel.DEBUG
  , logToFile = False
  , logToConsole = True
  , logRawSql = False
  , prettyPrinting = False
  }

let httpClientOptions =
  { timeoutMs = +2000
  , maxRetries = +3
  }

let ServerName = < APP_BACKEND | BECKN_TRANSPORT | DRIVER_OFFER_BPP >

in { smsSessionConfig = smsSessionConfig
   , autoMigrate = False
   , loggerConfig = loggerConfig
   , LogLevel = LogLevel
   , ExotelCfg = ExotelCfg
   , signatureExpiry = +600 -- in seconds
   , mkShard = mkShard
   , httpClientOptions = httpClientOptions
   , ServerName = ServerName
   }
