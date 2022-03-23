let globalCommon = ../generic/common.dhall

let appCfg = ./beckn-transport.dhall

let SortMode = < ETA | IdleTime >

let shards =
      [ globalCommon.mkShard
          +0
          "api.beckn.juspay.in/bpp/cab/v1/3c5fa6ae-2e90-4bb9-818e-7bb109b4cca3"
      ]

in  { appCfg = appCfg
    , defaultSortMode = SortMode.ETA
    , driverNotificationExpiry = +25
    , rideAllocationExpiry = +180
    , driverBatchSize = +5
    , requestsNumPerIteration = +50
    , processDelay = +1000
    , shards = shards
    , metricsPort = +9999
    , reallocationsLimit = +5
    , healthcheckPort = appCfg.bgtmPort
    , httpClientOptions = appCfg.httpClientOptions
    , dbCfg = appCfg.dbCfg
    , googleMapsUrl = appCfg.googleMapsUrl
    , googleMapsKey = appCfg.googleMapsKey
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
