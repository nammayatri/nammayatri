let common = ./common.dhall

let hcfg =
  { connectHost = "localhost"
  , connectPort = 6379
  , connectAuth = None Text
  , connectDatabase = +0
  , connectMaxConnections = +50
  , connectMaxIdleTime = +30
  , connectTimeout = None Integer
  }

in
{ port = +8026
, graceTerminationPeriod = +90
, hedisCfg = hcfg
, loggerConfig = common.loggerConfig // {logFilePath = "/tmp/mock-dunzo.log"}
}
