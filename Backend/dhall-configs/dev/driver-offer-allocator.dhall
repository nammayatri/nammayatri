let common = ../generic/common.dhall

let appCfg = ./dynamic-offer-driver-app.dhall

let sec = ./secrets/dynamic-offer-driver-app.dhall

let transporter = ./dynamic-offer-driver-app.dhall

let criticalAPIs =
      { criticalAPIList =
        [ "/ui/auth/"
        , "/ui/auth/:authId/verify/"
        , "/ui/auth/otp/:authId/resend/"
        , "/ui/driver/profile/"
        , "/ui/driver/location/"
        , "/ui/driver/location/:rideId/"
        , "/ui/driver/setActivity/"
        , "/ui/driver/searchRequest/quote/respond/"
        , "/internal/drivers/nearby/"
        , "/internal/drivers/location/"
        , "/internal/ride/rideDetails/"
        , "/internal/ride/:rideId/start/"
        , "/internal/ride/:rideId/end/"
        , "/beckn/:merchantId/search/"
        , "/beckn/:merchantId/select/"
        , "/beckn/:merchantId/init/"
        , "/beckn/:merchantId/confirm/"
        , "/beckn/:merchantId/cancel/"
        , "/beckn/:merchantId/status/"
        , "/ui/driver/ride/:rideId/cancel/"
        , "/ui/driver/otpRide/start/"
        , "/ui/driver/ride/:rideId/start/"
        , "/ui/driver/ride/:rideId/end/"
        , "/ui/driver/ride/:rideId/call/customer/"
        , "/ui/exotel/call/customer/number/"
        , "/ui/driver/cleardues/"
        , "/ui/payment/:invoiceId/createOrder/"
        , "/:merchantId/service/juspay/payment/"
        , "/ui/payment/:orderId/status/"
        , "/ui/driver/v2/payments/history/:invoiceId/entity/"
        ]
      }

let schedulerConfig =
      { loggerConfig =
              common.loggerConfig
          //  { logRawSql = True
              , logFilePath = "/tmp/driver-offer-scheduler.log"
              , prettyPrinting = True
              }
      , esqDBCfg = appCfg.esqDBCfg
      , metricsPort = +8056
      , hedisCfg = appCfg.hedisCfg
      , hedisClusterCfg = appCfg.hedisClusterCfg
      , hedisNonCriticalCfg = appCfg.hedisCfg
      , hedisNonCriticalClusterCfg = appCfg.hedisClusterCfg
      , hedisMigrationStage = True
      , cutOffHedisCluster = False
      , hedisPrefix = "driver-offer-scheduler"
      , port = +8055
      , loopIntervalSec = +5
      , expirationTime = +60
      , waitBeforeRetry = +1
      , tasksPerIteration = +20
      , graceTerminationPeriod = +10
      , enableRedisLatencyLogging = False
      , enablePrometheusMetricLogging = True
      , groupName = "myGroup"
      , schedulerType = common.schedulerType.RedisBased
      , schedulerSetName = "Scheduled_Jobs"
      , streamName = "Available_Jobs"
      , maxThreads = +10
      , block = +10000
      , readCount = +1
      , kafkaProducerCfg = appCfg.kafkaProducerCfg
      , criticalAPIs
      }

in  { appCfg =
            appCfg
        //  { loggerConfig =
                    appCfg.loggerConfig
                //  { logFilePath = "/tmp/driver-offer-allocator.log" }
            }
    , schedulerConfig
    }
