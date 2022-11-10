let common = ./common.dhall

let sec = ./secrets/beckn-transport.dhall

let GeoRestriction = < Unrestricted | Regions : List Text >

let postgresConfig =
      { connectHost = "beckn-integ-v2.ctiuwghisbi9.ap-south-1.rds.amazonaws.com"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_transporter_v2"
      }

let esqDBCfg =
      { connectHost = postgresConfig.connectHost
      , connectPort = postgresConfig.connectPort
      , connectUser = postgresConfig.connectUser
      , connectPassword = postgresConfig.connectPassword
      , connectDatabase = postgresConfig.connectDatabase
      , connectSchemaName = "atlas_transporter"
      }

let rcfg =
      { connectHost = "beckn-redis-001.zkt6uh.ng.0001.aps1.cache.amazonaws.com"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +1
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
  , url = "https://5vmxvj.api.infobip.com/sms/2/text/advanced"
  , sender = "JUSPAY"
  }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions = { limit = +20, limitResetTimeInSec = +40 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg =
      { brokers = [ "alpha-c1-kafka-bootstrap.strimzi.svc.cluster.local:9092" ]
      }

let cacheConfig = { configsExpTime = +86400 }

in  { esqDBCfg
    , hedisCfg = rcfg
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
    , inviteSmsTemplate =
        "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
    , port = +8014
    , metricsPort = +9999
    , hostName = "juspay.in"
    , nwAddress = "https://api.sandbox.beckn.juspay.in/dev/bpp/cab/v1"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , caseExpiry = Some +7200
    , exotelCfg = Some common.exotelCfg
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , coreVersion = "0.9.3"
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/beckn-transport.log" }
    , googleMapsUrl = "https://maps.googleapis.com/maps/api/"
    , googleMapsKey = common.googleMapsKey
    , fcmUrl = common.fcmUrl
    , fcmJsonPath = common.fcmJsonPath
    , fcmTokenKeyPrefix = "FIXME"
    , graceTerminationPeriod = +90
    , defaultRadiusOfSearch = +5000
    , driverPositionInfoExpiry = Some +600
    , apiRateLimitOptions
    , httpClientOptions = common.httpClientOptions
    , authTokenCacheExpiry = +600
    , minimumDriverRatesCount = +5
    , recalculateFareEnabled = True
    , metricsSearchDurationTimeout = +45
    , registryUrl = common.registryUrl
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , selfUIUrl = "https://api.sandbox.beckn.juspay.in/dev/bpp/cab/v2/"
    , schedulingReserveTime = +1800
    , driverEstimatedPickupDuration = +300
    , defaultPickupLocThreshold = +500
    , defaultDropLocThreshold = +500
    , defaultRideTravelledDistanceThreshold = +700
    , defaultRideTimeEstimatedThreshold = +900 --seconds
    , cacheConfig
    , dashboardToken = sec.dashboardToken
    , driverLocationUpdateRateLimitOptions
    , driverLocationUpdateNotificationTemplate = "Yatri: Location updates calls are exceeding for driver with {#driver-id#}."
    }
