let globalCommon = ../generic/common.dhall

let appCfg = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard
          +0
          "api.sandbox.beckn.juspay.in/bpp/cab/v1/3041599b-2fcf-45e1-bfd5-115db5cd1353"
      , globalCommon.mkShard
          +1
          "api.sandbox.beckn.juspay.in/bpp/cab/v1/87a04bab-bc3b-4d2a-866a-3c5ee9cc3b34"
      ]

in  { appCfg
    , metricsPort = +9999
    , reallocationsLimit = +5
    , defaultSortMode = SortMode.ETA
    , driverNotificationExpiry = +25
    , rideAllocationExpiry = +180
    , driverBatchSize = +5
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards
    , healthcheckPort = +8114
    , httpClientOptions = appCfg.httpClientOptions
    , esqDBCfg = appCfg.esqDBCfg
    , googleCfg = appCfg.googleCfg
    , hedisCfg = appCfg.hedisCfg
    , loggerConfig =
        appCfg.loggerConfig // { logFilePath = "/tmp/allocation-service.log" }
    , kafkaProducerCfg = appCfg.kafkaProducerCfg
    , nwAddress = appCfg.nwAddress
    , fcmUrl = appCfg.fcmUrl
    , fcmJsonPath = appCfg.fcmJsonPath
    , fcmTokenKeyPrefix = "transporter-allocator"
    , defaultRadiusOfSearch = appCfg.defaultRadiusOfSearch
    , driverPositionInfoExpiry = appCfg.driverPositionInfoExpiry
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , encTools = appCfg.encTools
    , selfUIUrl = appCfg.selfUIUrl
    , cacheConfig = appCfg.cacheConfig
    }
