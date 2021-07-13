let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
  [ globalCommon.mkShard +0 "JUSPAY.MOBILITY.PROVIDER.1.SANDBOX"
  , globalCommon.mkShard +1 "JUSPAY.MOBILITY.PROVIDER.2.SANDBOX"
  , globalCommon.mkShard +2 "JUSPAY.MOBILITY.PROVIDER.3.SANDBOX"
  , globalCommon.mkShard +3 "JUSPAY.MOBILITY.PROVIDER.4.SANDBOX"
  , globalCommon.mkShard +4 "JUSPAY.MOBILITY.PROVIDER.5.SANDBOX"
  , globalCommon.mkShard +5 "JUSPAY.MOBILITY.PROVIDER.6.SANDBOX"
  , globalCommon.mkShard +6 "JUSPAY.MOBILITY.PROVIDER.7.SANDBOX"
  , globalCommon.mkShard +7 "JUSPAY.MOBILITY.PROVIDER.12.SANDBOX"
  , globalCommon.mkShard +8 "JUSPAY.MOBILITY.PROVIDER.13.SANDBOX"
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
, metricsPort = +9999
, driverAllocationConfig = driverAllocationConfig
}
