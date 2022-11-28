let common = ../generic/common.dhall

let sec = ./secrets/beckn-transport.dhall

let transporter = ./beckn-transport.dhall

let JobType = < AllocateRental | FakeType >

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

in  { loggerConfig =
            common.loggerConfig
        //  { logRawSql = False
            , logFilePath = "/tmp/transporter-scheduler.log"
            , prettyPrinting = True
            }
    , esqDBCfg = transporter.esqDBCfg
    , metricsPort = +8054
    , hedisCfg = rcfg
    , hedisPrefix = "transporter-scheduler"
    , port = +8053
    , loopIntervalSec = +5
    , expirationTime = +60
    , waitBeforeRetry = +1
    , jobType = None JobType
    , tasksPerIteration = +20
    , graceTerminationPeriod = +10
    }
