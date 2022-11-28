let common = ./common.dhall

let sec = ./secrets/beckn-transport.dhall

let GeoRestriction = < Unrestricted | Regions : List Text >

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_transporter"
      }

let rcfg =
      { connectHost = "localhost"
      , connectPort = 6379
      , connectAuth = None Text
      , connectDatabase = +0
      , connectMaxConnections = +50
      , connectMaxIdleTime = +30
      , connectTimeout = None Integer
      }

let smsConfig =
      { sessionConfig = common.smsSessionConfig
      , credConfig =
        { username = common.smsUserName
        , password = common.smsPassword
        , otpHash = sec.smsOtpHash
        }
      , useFakeSms = Some 7891
      , url = "http://localhost:4343"
      , sender = "JUSPAY"
      }

let geofencingConfig =
      { origin = GeoRestriction.Regions [ "Ernakulam" ]
      , destination = GeoRestriction.Regions [ "Ernakulam", "Kerala" ]
      }

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://5vmxvj.api.infobip.com/sms/2/text/advanced"
      , sender = "JUSPAY"
      }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +100, limitResetTimeInSec = +1 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

let cacheConfig = { configsExpTime = +86400 }

in  { esqDBCfg
    , hedisCfg = rcfg
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
    , inviteSmsTemplate =
        "Welcome to the Yatri platform! Your agency ({#org#}) has added you as a driver. Start getting rides by installing the app: https://bit.ly/3wgLTcU"
    , port = +8014
    , metricsPort = +9997
    , hostName = "localhost"
    , nwAddress = "http://localhost:8014/v1/"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , caseExpiry = Some +7200
    , exotelCfg = Some common.exotelCfg
    , migrationPath = Some
        (   env:BECKN_TRANSPORT_MIGRATION_PATH as Text
          ? "dev/migrations/beckn-transport"
        )
    , autoMigrate = True
    , coreVersion = "0.9.3"
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/beckn-transport.log" }
    , fcmUrl = common.fcmUrl
    , fcmJsonPath = common.fcmJsonPath
    , fcmTokenKeyPrefix = "transporter-bpp"
    , graceTerminationPeriod = +90
    , defaultRadiusOfSearch = +5000
    , driverPositionInfoExpiry = Some +36000
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
    , selfUIUrl = "http://localhost:8014/v2/"
    , schedulingReserveTime = +1800
    , driverEstimatedPickupDuration = +300
    , dashboardToken = sec.dashboardToken
    , defaultPickupLocThreshold = +500
    , defaultDropLocThreshold = +500
    , defaultRideTravelledDistanceThreshold = +700
    , defaultRideTimeEstimatedThreshold = +900
    , cacheConfig
    , driverLocationUpdateRateLimitOptions
    , driverLocationUpdateNotificationTemplate =
        "Yatri: Location updates calls are exceeding for driver with {#driver-id#}."
    }
