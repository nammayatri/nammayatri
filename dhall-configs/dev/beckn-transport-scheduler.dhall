let becknTransport = ./beckn-transport.dhall

let JobType = < PrintBananasCount | PrintCurrentTimeWithErrorProbability | IncorrectDataJobType | FakeJobType >

in
{
loggerConfig = becknTransport.loggerConfig // { logRawSql = False } ,
esqDBCfg = becknTransport.esqDBCfg,
migrationPath = Some (env:BECKN_TRANSPORT_SCHEDULER_MIGRATION_PATH as Text ? "dev/migrations/scheduler"),
autoMigrate = True,
metricsPort = +8051,
hedisCfg = becknTransport.hcfg,
hedisPrefix = "beckn-transport-scheduler",
port = +8050,
loopIntervalSec = +5,
expirationTime = +60,
waitBeforeRetry = +1,
--jobType = Some JobType.PrintBananasCount
jobType = None JobType
}
