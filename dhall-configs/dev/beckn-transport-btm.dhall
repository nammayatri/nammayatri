let becknTransport = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let driverAllocationConfig =
  { defaultSortMode = SortMode.ETA
  , driverNotificationExpiry = +25 -- seconds
  , rideAllocationExpiry = +180 -- seconds
  , requestsNumPerIteration = +50
  , processDelay = +1 -- seconds
  }

let appCfg = becknTransport //
  { loggerConfig = becknTransport.loggerConfig //
    { logFilePath = "/tmp/beckn-transport-btm.log"
    }
  }

in

{ appCfg = appCfg
, metricsPort = +9996
, driverAllocationConfig = driverAllocationConfig
}