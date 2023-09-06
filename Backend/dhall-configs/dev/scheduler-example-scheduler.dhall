let common = ../generic/common.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = "atlas_scheduler_example_user"
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_scheduler_example"
      , connectionPoolCount = +25
      }

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

in  { loggerConfig =
            common.loggerConfig
        //  { logRawSql = True
            , logFilePath = "/tmp/scheduler-example-scheduler.log"
            , prettyPrinting = True
            }
    , esqDBCfg
    , metricsPort = +8052
    , hedisCfg = rcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = rcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , hedisPrefix = "example-scheduler"
    , port = +8051
    , loopIntervalSec = +5
    , expirationTime = +600
    , waitBeforeRetry = +1
    , tasksPerIteration = +20
    , graceTerminationPeriod = +10
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , groupName = "myGroup"
    , schedulerType = common.schedulerType.DbBased
    , schedulerSetName = "Scheduled_Jobs"
    , streamName = "Available_Jobs"
    }
