{
  port = +9090
-- for selfId see common.dhall, credRegistry, second arg of mkCredential
, selfId = "JUSPAY.MOBILITY.PROVIDER.UAT.2"
-- for uniqueKeyId see common.dhall, credRegistry, first arg of mkCredential
, uniqueKeyId = "juspay-mobility-bpp-1-key" 
, selfUri = "http://localhost:9090/"  -- public address of a node
, redisPrefix = "mock-parking-bpp"
, statusWaitTimeSec = +25
, callbackWaitTimeMilliSec = +500
}
