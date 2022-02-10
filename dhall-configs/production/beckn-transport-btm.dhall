let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
  [ globalCommon.mkShard +0 "api.beckn.juspay.in/bpp/cab/v1/3c5fa6ae-2e90-4bb9-818e-7bb109b4cca3"
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
, httpClientOptions = appCfg.httpClientOptions
}
