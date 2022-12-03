let globalCommon = ../generic/common.dhall

let appCfg = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard +0 "JUSPAY.MOBILITY.PROVIDER.UAT.1"
      , globalCommon.mkShard +1 "another-test-cabs"
      ]

in  { appCfg
    , metricsPort = +9996
    , reallocationsLimit = +5
    , defaultSortMode = SortMode.ETA
    , driverNotificationExpiry = +2500
    , rideAllocationExpiry = +180
    , driverBatchSize = +5
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards
    , healthcheckPort = +8114
    , httpClientOptions = appCfg.httpClientOptions
    , esqDBCfg = appCfg.esqDBCfg
    , esqDBReplicaCfg = appCfg.esqDBReplicaCfg
    , hedisCfg = appCfg.hedisCfg
    , loggerConfig =
            appCfg.loggerConfig
        //  { logFilePath = "/tmp/allocation-service.log", logRawSql = False }
    , kafkaProducerCfg = appCfg.kafkaProducerCfg
    , nwAddress = appCfg.nwAddress
    , defaultRadiusOfSearch = appCfg.defaultRadiusOfSearch
    , driverPositionInfoExpiry = appCfg.driverPositionInfoExpiry
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , encTools = appCfg.encTools
    , selfUIUrl = appCfg.selfUIUrl
    , cacheConfig = appCfg.cacheConfig
    }
