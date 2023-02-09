let common = ../generic/common.dhall

let sec = ./secrets/static-offer-driver-app.dhall

let postgresConfig =
      { connectHost = "adb.primary.beckn.juspay.net"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_transporter"
      }

let esqDBCfg =
      { connectHost = postgresConfig.connectHost
      , connectPort = postgresConfig.connectPort
      , connectUser = postgresConfig.connectUser
      , connectPassword = postgresConfig.connectPassword
      , connectDatabase = postgresConfig.connectDatabase
      , connectSchemaName = "atlas_transporter"
      }

let rcfg =
      { connectHost = "cache.primary.beckn.juspay.net"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

in  { loggerConfig =
            common.loggerConfig
        //  { logRawSql = False
            , logFilePath = "/tmp/transporter-scheduler.log"
            }
    , esqDBCfg
    , metricsPort = +8054
    , hedisCfg = rcfg
    , hedisPrefix = "transporter-scheduler"
    , port = +8053
    , loopIntervalSec = +5
    , expirationTime = +60
    , waitBeforeRetry = +1
    , tasksPerIteration = +20
    , graceTerminationPeriod = +1
    }
