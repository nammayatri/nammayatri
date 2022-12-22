let common = ../generic/common.dhall

let appCfg = ./driver-offer-bpp.dhall

let sec = ./secrets/beckn-transport.dhall

let transporter = ./beckn-transport.dhall

let schedulerConfig =
      { loggerConfig =
              common.loggerConfig
          //  { logRawSql = False
              , logFilePath = "/tmp/driver-offer-scheduler.log"
              , prettyPrinting = True
              }
      , esqDBCfg = appCfg.esqDBCfg
      , metricsPort = +8056
      , hedisCfg = appCfg.hedisCfg
      , hedisPrefix = "driver-offer-scheduler"
      , port = +8055
      , loopIntervalSec = +5
      , expirationTime = +60
      , waitBeforeRetry = +1
      , jobType = None {}
      , tasksPerIteration = +20
      , graceTerminationPeriod = +10
      }

let PoolSortingType = < ByAcceptanceRatio | ByRandom >

let driverPoolBatchesCfg =
      { driverBatchSize = +20
      , maxNumberOfBatches = +1
      , poolSortingType = PoolSortingType.ByRandom
      }

in  { appCfg =
            appCfg
        //  { loggerConfig =
                    appCfg.loggerConfig
                //  { logFilePath = "/tmp/driver-offer-allocator.log" }
            }
    , schedulerConfig
    , driverPoolBatchesCfg
    , singleBatchProcessTime = +30
    }
