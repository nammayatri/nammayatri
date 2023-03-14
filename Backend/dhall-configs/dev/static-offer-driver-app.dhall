let common = ./common.dhall

let sec = ./secrets/static-offer-driver-app.dhall

let GeoRestriction = < Unrestricted | Regions : List Text >

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_transporter"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5435
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      }

let clickhouseCfg =
      { username = sec.clickHouseUsername
      , host = "xxxxx"
      , port = 1234
      , password = sec.clickHousePassword
      , database = "xxxx"
      , tls = True
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
      , url = "https://gye1yw.api.infobip.com"
      , webhookurl = "http://localhost:8014/v2/update/status"
      , sender = "JUSPAY"
      }

let WebengageConfig = { url = "https://st.in.webengage.com" }

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let driverLocationUpdateRateLimitOptions =
      { limit = +100, limitResetTimeInSec = +1 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

let cacheConfig = { configsExpTime = +86400 }

let driverPoolCfg =
      { defaultRadiusOfSearch = +5000, driverPositionInfoExpiry = Some +36000 }

let endRideDefCfg =
      { pickupLocThreshold = +500
      , dropLocThreshold = +500
      , rideTravelledDistThresholdWhenPickupOrDestIsDiff = +700
      , rideTravelledDistThresholdWhenPickupAndDestIsSame = +1200
      , rideTimeEstimatedThreshold = +900
      , waitingTimeEstimatedThreshold = +3
      }

in  { esqDBCfg
    , esqDBReplicaCfg
    , clickhouseCfg
    , hedisCfg = rcfg
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , webengageCfg = WebengageConfig
    , port = +8014
    , metricsPort = +9997
    , hostName = "localhost"
    , nwAddress = "http://localhost:8014/v1/"
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , searchExpiry = Some +7200
    , exotelCfg = Some common.exotelCfg
    , migrationPath = Some
        (   env:STATIC_OFFER_DRIVER_MIGRATION_PATH as Text
          ? "dev/migrations/static-offer-driver-app"
        )
    , autoMigrate = True
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
    , selfUIUrl = "http://localhost:8014/v2/"
    , schedulingReserveTime = +1800
    , driverEstimatedPickupDuration = +300
    , dashboardToken = sec.dashboardToken
    , driverReachedDistance = +100
    , defaultEndRideCfg = endRideDefCfg
    , cacheConfig
    , driverLocationUpdateRateLimitOptions
    , driverPoolCfg
    , driverLocationUpdateTopic = "location-updates"
    , snapToRoadSnippetThreshold = +300
    , appPrefix = "static-offer-driver-app"
    }
