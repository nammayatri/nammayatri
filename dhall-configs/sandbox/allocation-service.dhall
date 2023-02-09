let globalCommon = ../generic/common.dhall

let appCfg = ./static-offer-driver-app.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard
          +0
          "api.sandbox.beckn.juspay.in/bpp/cab/v1/3041599b-2fcf-45e1-bfd5-115db5cd1353"
      , globalCommon.mkShard
          +1
          "api.sandbox.beckn.juspay.in/bpp/cab/v1/87a04bab-bc3b-4d2a-866a-3c5ee9cc3b34"
      ]

let driverPoolBatchesCfg = { driverBatchSize = +5, maxNumberOfBatches = +3 }

in  { appCfg
    , metricsPort = +9999
    , reallocationsLimit = +5
    , driverNotificationExpiry = +25
    , rideAllocationExpiry = +180
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards
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
