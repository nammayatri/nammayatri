let common = ../generic/common.dhall
let sec = ./secrets/beckn-transport.dhall

let JobType = < AllocateRental | FakeType >

let postgresConfig =
  { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
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
  { connectHost = "beckn-redis-001-001.zkt6uh.0001.aps1.cache.amazonaws.com"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +1
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = Some +100
  }

in
{ loggerConfig = common.loggerConfig // { logRawSql = False, logFilePath = "/tmp/transporter-scheduler.log" }
, esqDBCfg = esqDBCfg
, metricsPort = +8054
, hedisCfg = rcfg
, hedisPrefix = "transporter-scheduler"
, port = +8053
, loopIntervalSec = +5
, expirationTime = +60
, waitBeforeRetry = +1
, jobType = None JobType
, tasksPerIteration = +20
, graceTerminationPeriod = +1
}
