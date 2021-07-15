let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
  [ globalCommon.mkShard +0 "JUSPAY.MOBILITY.PROVIDER.1.PROD"
  , globalCommon.mkShard +1 "JUSPAY.MOBILITY.PROVIDER.12.PROD"
  , globalCommon.mkShard +2 "JUSPAY.MOBILITY.PROVIDER.13.PROD"
  , globalCommon.mkShard +3 "JUSPAY.MOBILITY.PROVIDER.14.PROD"
  , globalCommon.mkShard +4 "JUSPAY.MOBILITY.PROVIDER.15.PROD"
  , globalCommon.mkShard +5 "JUSPAY.MOBILITY.PROVIDER.16.PROD"
  ]

let appCfg = becknTransport //
  { loggerConfig = becknTransport.loggerConfig //
    { logFilePath = "/tmp/beckn-transport-btm.log"
    }
  }

let driverAllocationConfig =
  { defaultSortMode = SortMode.ETA
  , driverNotificationExpiry = +25 -- seconds
  , rideAllocationExpiry = +180 -- seconds
  , requestsNumPerIteration = +50
  , processDelay = +1 -- seconds
  , shards = shards
  }

in

{ appCfg = appCfg
, metricsPort = +9999
, driverAllocationConfig = driverAllocationConfig
}
