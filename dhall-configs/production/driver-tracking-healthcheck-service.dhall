let globalCommon = ../generic/common.dhall

let appCfg = ./beckn-transport.dhall

in  { driverAllowedDelay = +300
    , notificationMinDelay = +50000
    , metricsPort = +9997
    , healthcheckPort = +8115
    , loggerConfig = appCfg.loggerConfig //  { logFilePath = "/tmp/driver-tracking-healthcheck.log" }
    , httpClientOptions = appCfg.httpClientOptions
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , redisCfg = appCfg.redisCfg
    , esqDBCfg = appCfg.esqDBCfg
    , fcmUrl = appCfg.fcmUrl
    , fcmJsonPath = appCfg.fcmJsonPath
    , fcmTokenKeyPrefix = "transporter-healthcheck"
    , encTools = appCfg.encTools
    , driverInactiveDelay = +86400
    , smsCfg =  appCfg.smsCfg
    , driverInactiveSmsTemplate = "Alert! You have been marked Busy on Yatri Partner, as we have not received any location update from your phone in more than a day. Please open the app and update your location for the app to work properly."
    }
