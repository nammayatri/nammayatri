let common = ./common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let base = ./kafka-consumers-base.dhall

let enabledMerchantCityIds = [] : List Text

let healthCheckAppCfg =
      { graceTerminationPeriod = appCfg.graceTerminationPeriod
      , healthcheckPort = +8115
      , notificationMinDelay = +60000000
      , driverInactiveDelay = +86400
      , smsCfg = appCfg.smsCfg
      , driverInactiveSmsTemplate =
          "Alert! You have been marked Busy on Namma Yatri Partner, as we have not received any location update from your phone in more than a day. Please open the app and update your location for the app to work properly."
      , driverAllowedDelayForLocationUpdateInSec = +10
      , driverLocationHealthCheckIntervalInSec = +60
      , fcmNofificationSendCount = +2
      , loggerConfig =
              appCfg.loggerConfig
          //  { logFilePath = "/tmp/driver-tracking-healthcheck.log" }
      , batchSize = +100
      , numberOfShards = +10
      , enabledMerchantCityIds
      }

in      base
    //  { transport = common.transportKind.Kafka
        , kafkaConsumerCfg =
            base.kafkaConsumerCfg // { topicNames = [ "location-updates" ] }
        , healthCheckAppCfg = Some healthCheckAppCfg
        , metricsPort = Natural/toInteger (env:METRICS_PORT ? 9994)
        }
