let common = ../generic/common.dhall
let sec = ./secrets/beckn-transport.dhall

let JobType = < PrintBananasCount | PrintCurrentTimeWithErrorProbability | IncorrectDataJobType | FakeJobType | TestTermination >

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
--  , connectUser = postgresConfig.connectUser
  , connectUser = "atlas_scheduler_example_user"
  , connectPassword = postgresConfig.connectPassword
  , connectDatabase = postgresConfig.connectDatabase
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
migrationPath = Some (env:BECKN_TRANSPORT_SCHEDULER_MIGRATION_PATH as Text ? "dev/migrations/scheduler"),
autoMigrate = True,
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
