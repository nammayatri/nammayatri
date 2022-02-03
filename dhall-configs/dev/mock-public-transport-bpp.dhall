let common = ./common.dhall

in
{
  port = +9091
-- for selfId see common.dhall, credRegistry, second arg of mkCredential
--, selfId = "JUSPAY.MOBILITY.PROVIDER.UAT.3"
  , selfId = "public-transport-bpp"
-- for uniqueKeyId see common.dhall, credRegistry, first arg of mkCredential
, uniqueKeyId = "juspay-mobility-bpp-1-key1" 
, selfUri = "http://localhost:9091/"  -- public address of a node
, redisPrefix = "mock-public-transport-bpp"
, statusWaitTimeSec = +25
, callbackWaitTimeMilliSec = +500
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-parking-bpp.log"} 
}
