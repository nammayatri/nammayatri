let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
  [ globalCommon.mkShard +0 "JUSPAY.MOBILITY.PROVIDER.1.DEV"
  , globalCommon.mkShard +1 "JUSPAY.MOBILITY.PROVIDER.2.DEV"
  , globalCommon.mkShard +2 "JUSPAY.MOBILITY.PROVIDER.3.DEV"
  , globalCommon.mkShard +3 "JUSPAY.MOBILITY.PROVIDER.4.DEV"
  , globalCommon.mkShard +4 "JUSPAY.MOBILITY.PROVIDER.5.DEV"
  , globalCommon.mkShard +5 "JUSPAY.MOBILITY.PROVIDER.6.DEV"
  , globalCommon.mkShard +6 "JUSPAY.MOBILITY.PROVIDER.7.DEV"
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
