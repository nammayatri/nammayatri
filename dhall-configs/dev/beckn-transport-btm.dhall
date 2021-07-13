let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
 [ globalCommon.mkShard +0 "JUSPAY.MOBILITY.PROVIDER.UAT.1"
 , globalCommon.mkShard +1 "another-test-cabs"
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
  , processDelay = +1000 -- ms
  , shards = shards
  }

in

{ appCfg = appCfg
, metricsPort = +9996
, driverAllocationConfig = driverAllocationConfig
}
