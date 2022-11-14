let common = ../generic/common.dhall
let sec = ./secrets/beckn-transport.dhall

let JobType = < PrintBananasCount | PrintCurrentTimeWithErrorProbability | IncorrectDataJobType | FakeJobType | TestTermination >

let esqDBCfg =
  { connectHost = "localhost"
  , connectPort = 5434
  , connectUser = "atlas_scheduler_example_user"
  , connectPassword = sec.dbPassword
  , connectDatabase = "atlas_dev"
  , connectSchemaName = "atlas_scheduler_example"
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
loggerConfig = common.loggerConfig // { logRawSql = False, logFilePath = "/tmp/scheduler-example-scheduler.log", prettyPrinting = True } ,
esqDBCfg = esqDBCfg,
metricsPort = +8052,
hedisCfg = rcfg,
hedisPrefix = "example-scheduler",
port = +8051,
loopIntervalSec = +5,
expirationTime = +600,
waitBeforeRetry = +1,
--jobType = Some JobType.PrintBananasCount
jobType = None JobType,
tasksPerIteration = +20,
graceTerminationPeriod = +10
}
