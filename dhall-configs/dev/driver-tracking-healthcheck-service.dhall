let globalCommon = ../generic/common.dhall

let appCfg = ./beckn-transport.dhall

in  { driverAllowedDelay = +300
    , notificationMinDelay = +50000
    , metricsPort = +9994
    , healthcheckPort = +8115
    , loggerConfig = appCfg.loggerConfig
              //  { logFilePath = "/tmp/driver-tracking-healthcheck.log" }
    , httpClientOptions = appCfg.httpClientOptions
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , redisCfg = appCfg.redisCfg
    , esqDBCfg = appCfg.esqDBCfg
    , nwAddress = appCfg.nwAddress
    , fcmJsonPath = appCfg.fcmJsonPath
    , fcmUrl = appCfg.fcmUrl
    , encTools = appCfg.encTools
    }
