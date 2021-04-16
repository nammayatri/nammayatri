let mkCredential =
 \(uniqueKeyId : Text) -> \(shortOrgId : Text) -> \(signPubKey : Text) ->
  {
    uniqueKeyId = uniqueKeyId
  , shortOrgId = shortOrgId
  , signPubKey = signPubKey
  }

let mkSigningKey =
 \(uniqueKeyId : Text) -> \(signPrivKey : Text) ->
  {
    uniqueKeyId = uniqueKeyId
  , signPrivKey = signPrivKey
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
  , isAsync = True
  , logToFile = True
  , logToConsole = True
  , logRawSql = True
  }

in { defaultPoolConfig = defaultPoolConfig
   , smsSessionConfig = smsSessionConfig
   , autoMigrate = False
   , loggerConfig = loggerConfig
   , LogLevel = LogLevel
   , ExotelCfg = ExotelCfg
   , signatureExpiry = +600 -- in seconds
   , mkCredential = mkCredential
   , mkSigningKey = mkSigningKey
   }
