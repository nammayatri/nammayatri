let globalCommon = ../generic/common.dhall

let appCfg = ./static-offer-driver-app.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard
          +0
          "api.beckn.juspay.in/bpp/cab/v1/3c5fa6ae-2e90-4bb9-818e-7bb109b4cca3"
      ]

let driverPoolBatchesCfg = { driverBatchSize = +5, maxNumberOfBatches = +3 }

in  { appCfg
    , driverNotificationExpiry = +25
    , rideAllocationExpiry = +120
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards
    , metricsPort = +9999
    , reallocationsLimit = +5
    , healthcheckPort = +8114
    , httpClientOptions = appCfg.httpClientOptions
    , shortDurationRetryCfg = appCfg.shortDurationRetryCfg
    , longDurationRetryCfg = appCfg.longDurationRetryCfg
    , esqDBCfg = appCfg.esqDBCfg
    , hedisCfg = appCfg.hedisCfg
    , loggerConfig =
        appCfg.loggerConfig // { logFilePath = "/tmp/allocation-service.log" }
    , kafkaProducerCfg = appCfg.kafkaProducerCfg
    , nwAddress = appCfg.nwAddress
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , encTools = appCfg.encTools
    , selfUIUrl = appCfg.selfUIUrl
    , cacheConfig = appCfg.cacheConfig
    , driverPoolCfg = appCfg.driverPoolCfg
    , driverPoolBatchesCfg
    }
