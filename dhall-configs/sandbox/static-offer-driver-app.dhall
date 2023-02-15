let common = ./common.dhall

let sec = ./secrets/static-offer-driver-app.dhall

let GeoRestriction = < Unrestricted | Regions : List Text >

let esqDBCfg =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_transporter"
      , connectSchemaName = "atlas_transporter"
      }

let esqDBReplicaCfg =
      { connectHost =
          "beckn-integ-v2-r1.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = esqDBCfg.connectPort
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let rcfg =
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +2
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = Some +100
      }

let smsConfig =
      { sessionConfig = common.smsSessionConfig
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        }
      , useFakeSms = Some 7891
      , url = "https://http.myvfirst.com"
      , sender = "JUSPAY"
      }

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://gye1yw.api.infobip.com"
      , sender = "JUSPAY"
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +10, limitResetTimeInSec = +40 }

let httpClientOptions = { timeoutMs = +2000 }

let shortDurationRetryCfg = { maxRetries = +3, baseCoefficient = +2 }

let longDurationRetryCfg = { maxRetries = +3, baseCoefficient = +4 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [ "kafka.kafka.svc.cluster.local:9092" ] }

let cacheConfig = { configsExpTime = +86400 }

let driverPoolCfg =
      { defaultRadiusOfSearch = +5000, driverPositionInfoExpiry = Some +600 }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = rcfg
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
    , inviteSmsTemplate =
        "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
    , port = +8014
    , metricsPort = +9999
    , hostName = "juspay.in"
    , nwAddress = "https://api.sandbox.beckn.juspay.in/bpp/cab/v1"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , searchExpiry = Some +7200
    , exotelCfg = Some common.exotelCfg
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , coreVersion = "0.9.3"
    , loggerConfig =
            common.loggerConfig
        //  { logFilePath = "/tmp/static-offer-driver-app.log" }
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , minimumDriverRatesCount = +5
    , recalculateFareEnabled = True
    , metricsSearchDurationTimeout = +45
    , registryUrl = common.registryUrl
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , selfUIUrl = "https://api.sandbox.beckn.juspay.in/bpp/cab/v2/"
    , schedulingReserveTime = +1800
    , driverEstimatedPickupDuration = +300
    , defaultPickupLocThreshold = +500
    , defaultDropLocThreshold = +500
    , defaultrideTravelledDistThresholdWhenPickupOrDestIsDiff = +700
    , defaultrideTravelledDistThresholdWhenPickupAndDestIsSame = +1200
    , defaultRideTimeEstimatedThreshold = +900
    , defaultWaitingTimeEstimatedThreshold = +3
    , cacheConfig
    , dashboardToken = sec.dashboardToken
    , driverLocationUpdateRateLimitOptions
    , driverReachedDistance = +100
    , driverLocationUpdateNotificationTemplate =
        "Yatri: Location updates calls are exceeding for driver with {#driver-id#}."
    , driverPoolCfg
    , driverLocationUpdateTopic = "location-updates-sandbox"
    , snapToRoadSnippetThreshold = common.snapToRoadSnippetThreshold
    }
