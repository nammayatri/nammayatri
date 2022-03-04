let common = ./common.dhall
let sec = ./secrets/public-transport-bap.dhall

let hcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

in
{
  port = +8091
, selfId = "mock-public-transport-bpp"
, uniqueKeyId = "juspay-mobility-bpp-1-key1"
, selfUri = "http://localhost:8091/"  -- public address of a node
, hedisCfg = hcfg
, statusWaitTimeSec = +25
, callbackWaitTimeMilliSec = +500
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-parking-bpp.log"}
, authEntity =
  { signingKey = sec.signingKey
  , uniqueKeyId = "juspay-mobility-bpp-1-key1"
  , signatureExpiry = common.signatureExpiry
  }
}
