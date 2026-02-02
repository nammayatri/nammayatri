let common = ../generic/common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let transporter = ./dynamic-offer-driver-app.dhall

let inMemConfig = { enableInMem = True, maxInMemSize = +100000000 }

let schedulerConfig =
      { loggerConfig =
              common.loggerConfig
          //  { logRawSql = True
              , logFilePath = "/tmp/driver-offer-scheduler.log"
              , prettyPrinting = True
              }
      , esqDBCfg = appCfg.esqDBCfg
      , esqDBReplicaCfg = appCfg.esqDBReplicaCfg
      , metricsPort = +8056
      , hedisCfg = appCfg.hedisCfg
      , hedisClusterCfg = appCfg.hedisClusterCfg
      , hedisNonCriticalCfg = appCfg.hedisCfg
      , hedisNonCriticalClusterCfg = appCfg.hedisClusterCfg
      , hedisMigrationStage = False
      , cutOffHedisCluster = False
      , hedisPrefix = "driver-offer-scheduler"
      , port = +8055
      , loopIntervalSec = +5
      , expirationTime = +60
      , waitBeforeRetry = +1
      , tasksPerIteration = +20
      , graceTerminationPeriod = +10
      , enableRedisLatencyLogging = False
      , enablePrometheusMetricLogging = True
      , groupName = "myGroup"
      , schedulerType = common.schedulerType.RedisBased
      , schedulerSetName = "Scheduled_Jobs"
      , streamName = "Available_Jobs"
      , maxThreads = +10
      , block = +10000
      , readCount = +1
      , kafkaProducerCfg = appCfg.kafkaProducerCfg
      , cacConfig = appCfg.cacConfig
      , inMemConfig
      , hedisSecondaryClusterCfg = appCfg.hedisSecondaryClusterCfg
      , blackListedJobs = [] : List Text
      }

in  { appCfg =
            appCfg
        //  { loggerConfig =
                    appCfg.loggerConfig
                //  { logFilePath = "/tmp/driver-offer-allocator.log" }
            , cityDBSchema = "atlas_driver_offer_bpp" -- Note : For Production, verify the schema exists with `merchant_operating_city` table in the DB host specified in the esqDBCfg
            }
    , schedulerConfig
    }
