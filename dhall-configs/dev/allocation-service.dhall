let globalCommon = ../generic/common.dhall

let appCfg = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard +0 "JUSPAY.MOBILITY.PROVIDER.UAT.1"
      , globalCommon.mkShard +1 "another-test-cabs"
      ]

in  { appCfg = appCfg
    , metricsPort = +9996
    , reallocationsLimit = +5
    , defaultSortMode = SortMode.ETA
    , driverNotificationExpiry = +25
    , rideAllocationExpiry = +180
    , driverBatchSize = +5
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards = shards
    , healthcheckPort = appCfg.bgtmPort
    , httpClientOptions = appCfg.httpClientOptions
    , dbCfg = appCfg.dbCfg
    , redisCfg = appCfg.redisCfg
    , loggerConfig = appCfg.loggerConfig
              //  { logFilePath = "/tmp/allocation-service.log" }
    , kafkaProducerCfg = appCfg.kafkaProducerCfg
    , nwAddress = appCfg.nwAddress
    , fcmJsonPath = appCfg.fcmJsonPath
    , fcmUrl = appCfg.fcmUrl
    , exotelCfg = appCfg.exotelCfg
    , defaultRadiusOfSearch = appCfg.defaultRadiusOfSearch
    , driverPositionInfoExpiry = appCfg.driverPositionInfoExpiry
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , encTools = appCfg.encTools
    }
