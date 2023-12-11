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

let kafkaConsumerCfgs =
      { publicTransportQuotes =
        { brokers = [ "localhost:29092" ]
        , groupId = "publicTransportQuotesGroup"
        , timeoutMilliseconds = +10000
        , kafkaCompression = common.kafkaCompression.LZ4
        }
      }

let rccfg =
      { connectHost = "localhost"
      , connectPort = 30001
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let criticalAPIs =
      { criticalAPIList =
        [ "/v2/auth/"
        , "/v2/auth/signature/"
        , "/v2/auth/:authId/verify/"
        , "/v2/profile/"
        , "/v2/serviceability (/origin,/destination)"
        , "/v2/maps/getPlacename"
        , "/v2/maps /autocomplete"
        , "/v2/rideSearch/"
        , "/v2/rideSearch/:searchId/results/"
        , "/v2/estimate/:estimateId/results/"
        , "/v2/estimate/:estimateId/select2/"
        , "/v2/estimate/:estimateId/quotes/"
        , "/v2/estimate/:estimateId/cancel/"
        , "/v2/rideSearch/quotes/:quoteId/confirm/"
        , "/v2/rideBooking/:rideBookingId/"
        , "/v2/rideBooking/:rideBookingId/cancel/c"
        , "/v2/ride/:rideId/driver/location/"
        , "/beckn/cab/v1/:merchantId/on_search/"
        , "/beckn/cab/v1/:merchantId/on_select/"
        , "/beckn/cab/v1/:merchantId/on_init/"
        , "/beckn/cab/v1/:merchantId/on_confirm/"
        , "/beckn/cab/v1/:merchantId/on_track/"
        , "/beckn/cab/v1/:merchantId/on_status/"
        , "/v2/ride/:rideId/call/driver/"
        , "/v2/exotel/call/"
        , "/v2/sos/"
        ]
      }

in  { port = +8025
    , graceTerminationPeriod = +90
    , hedisCfg = hcfg
    , hedisClusterCfg = rccfg
    , hedisNonCriticalCfg = hcfg
    , hedisNonCriticalClusterCfg = rccfg
    , hedisMigrationStage = True
    , cutOffHedisCluster = True
    , kafkaConsumerCfgs
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/search-result-aggregator.log" }
    , enableRedisLatencyLogging = True
    , enablePrometheusMetricLogging = True
    , criticalAPIs
    }
