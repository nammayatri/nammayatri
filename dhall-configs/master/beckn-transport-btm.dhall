let globalCommon = ../generic/common.dhall
let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
  [ globalCommon.mkShard +0 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/565db72a-04d4-4211-90ae-c956461397b2"
  , globalCommon.mkShard +1 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/a45f243f-9915-4842-b78b-6d718844a48d"
  , globalCommon.mkShard +2 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/fb6ee235-8cf5-4f8f-aba2-40de1fa733d1"
  , globalCommon.mkShard +3 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/c26001a5-8a20-4e77-bebd-f9d7fce618bc"
  , globalCommon.mkShard +4 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/384786e9-63e1-4f00-bbd9-40480387907d"
  , globalCommon.mkShard +5 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/dc46e80a-99d7-4f96-9949-2c045106b081"
  , globalCommon.mkShard +6 "api.sandbox.beckn.juspay.in/latest/bpp/cab/v1/092ef105-6fe6-4eab-9c6f-e8a57b51e1af"
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
  , reallocationsLimit = +5
  }

in

{ appCfg = appCfg
, metricsPort = +9999
, driverAllocationConfig = driverAllocationConfig
, httpClientOptions = appCfg.httpClientOptions
}
