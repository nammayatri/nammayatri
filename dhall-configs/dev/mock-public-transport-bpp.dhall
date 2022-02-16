let common = ./common.dhall

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
  port = +9091
-- for selfId see common.dhall, credRegistry, second arg of mkCredential
  , selfId = "mock-public-transport-bpp"
-- for uniqueKeyId see common.dhall, credRegistry, first arg of mkCredential
, uniqueKeyId = "juspay-mobility-bpp-1-key1"
, selfUri = "http://localhost:9091/"  -- public address of a node
, hedisCfg = hcfg
, statusWaitTimeSec = +25
, callbackWaitTimeMilliSec = +500
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-parking-bpp.log"}
}