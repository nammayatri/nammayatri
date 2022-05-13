let common = ../generic/common.dhall
let sec = ./secrets/beckn-transport.dhall

let JobType = < AllocateRental | FakeType >

let postgresConfig =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = sec.dbUserId
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
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
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

in
{
loggerConfig = common.loggerConfig // { logRawSql = False, logFilePath = "/tmp/transporter-scheduler.log", prettyPrinting = True } ,
esqDBCfg = esqDBCfg,
metricsPort = +8054,
hedisCfg = rcfg,
hedisPrefix = "transporter-scheduler",
port = +8053,
loopIntervalSec = +5,
expirationTime = +60,
waitBeforeRetry = +1,
jobType = None JobType,
tasksPerIteration = +20,
graceTerminationPeriod = +10
}
