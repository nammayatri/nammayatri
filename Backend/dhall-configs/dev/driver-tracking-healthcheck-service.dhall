let globalCommon = ../generic/common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

in  { driverAllowedDelayForLocationUpdateInSec = +60
    , driverLocationHealthCheckIntervalInSec = +120
    , driverAppName = "dynamic-offer-driver-app"
    , notificationMinDelay = +50000
    , metricsPort = +9994
    , healthcheckPort = +8115
    , loggerConfig =
            appCfg.loggerConfig
        //  { logFilePath = "/tmp/driver-tracking-healthcheck.log" }
    , httpClientOptions = appCfg.httpClientOptions
    , shortDurationRetryCfg = appCfg.shortDurationRetryCfg
    , longDurationRetryCfg = appCfg.longDurationRetryCfg
    , graceTerminationPeriod = appCfg.graceTerminationPeriod
    , hedisCfg = appCfg.hedisCfg
    , hedisClusterCfg = appCfg.hedisClusterCfg
    , hedisNonCriticalCfg = appCfg.hedisCfg
    , hedisNonCriticalClusterCfg = appCfg.hedisClusterCfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , esqDBCfg = appCfg.esqDBCfg
    , encTools = appCfg.encTools
    , driverInactiveDelay = +86400
    , smsCfg = appCfg.smsCfg
    , driverInactiveSmsTemplate =
        "Alert! You have been marked Busy on Namma Yatri Partner, as we have not received any location update from your phone in more than a day. Please open the app and update your location for the app to work properly."
    , cacheConfig = appCfg.cacheConfig
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    }
