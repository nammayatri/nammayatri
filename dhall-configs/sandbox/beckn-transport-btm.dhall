let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
  [ globalCommon.mkShard +0 "api.sandbox.beckn.juspay.in/bpp/cab/v1/3041599b-2fcf-45e1-bfd5-115db5cd1353"
  , globalCommon.mkShard +1 "api.sandbox.beckn.juspay.in/bpp/cab/v1/87a04bab-bc3b-4d2a-866a-3c5ee9cc3b34"
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
  , driverBatchSize = +5
  , requestsNumPerIteration = +50
  , processDelay = +1000 -- ms
  , shards = shards
  }

in

{ appCfg = appCfg
, metricsPort = +9999
, driverAllocationConfig = driverAllocationConfig
}
