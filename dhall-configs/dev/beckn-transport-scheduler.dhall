let becknTransport = ./beckn-transport.dhall

in
{
loggerConfig = becknTransport.loggerConfig ,
esqDBCfg = becknTransport.esqDBCfg,
hedisCfg = becknTransport.hcfg,
hedisPrefix = "beckn-transport-scheduler:",
port = +8050,
loopIntervalSec = +5,
expirationTime = +60,
waitBeforeRetry = +1
}
