let common = ./common.dhall

let sec = ./secrets/rider-app.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_app"
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5435
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
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

let hcfg =
      { connectHost = rcfg.connectHost
      , connectPort = rcfg.connectPort
      , connectAuth = rcfg.connectAuth
      , connectDatabase = rcfg.connectDatabase
      , connectMaxConnections = rcfg.connectMaxConnections
      , connectMaxIdleTime = rcfg.connectMaxIdleTime
      , connectTimeout = rcfg.connectTimeout
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

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://gye1yw.api.infobip.com"
      , webhookurl = "http://localhost:8013/v2/update/status"
      , sender = "JUSPAY"
      }

let WebengageConfig = { url = "https://st.in.webengage.com" }

let gwUri = "http://localhost:8015/v1"

let apiRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let searchRateLimitOptions = { limit = +8000, limitResetTimeInSec = +1 }

let slackCfg =
      { channelName = "#beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [ "localhost:29092" ] }

let rideConfig =
      { driverReachedDistance = +100, driverOnTheWayNotifyExpiry = +3600 }

let cacheConfig = { configsExpTime = +86400 }

let cacheTranslationConfig = { expTranslationTime = +3600 }

in  { esqDBCfg
    , esqDBReplicaCfg
    , hedisCfg = hcfg
    , smsCfg = smsConfig
    , infoBIPCfg = InfoBIPConfig
    , webengageCfg = WebengageConfig
    , port = +8013
    , metricsPort = +9999
    , hostName = "localhost"
    , selfUIUrl = "http://localhost:8013/v2/"
    , bapSelfIds =
      { cabs = "JUSPAY.MOBILITY.APP.UAT.1"
      , metro = "JUSPAY.MOBILITY.APP.UAT.2"
      }
    , bapSelfURIs =
      { cabs = "http://localhost:8013/cab/v1/"
      , metro = "http://localhost:8013/metro/v1/"
      }
    , bapSelfUniqueKeyIds =
      { cabs = "juspay-mobility-bap-1-key"
      , metro = "juspay-mobility-bap-1-key"
      }
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , searchRequestExpiry = Some +600
    , migrationPath = Some
        (env:RIDER_APP_MIGRATION_PATH as Text ? "dev/migrations/rider-app")
    , autoMigrate = True
    , coreVersion = "0.9.3"
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/rider-app.log" }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , metricsSearchDurationTimeout = +45
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , searchRateLimitOptions
    , slackCfg
    , searchLimitExceedNotificationTemplate =
        "Customer with {#cust-id#} is exceeding the search limit."
    , httpClientOptions = common.httpClientOptions
    , shortDurationRetryCfg = common.shortDurationRetryCfg
    , longDurationRetryCfg = common.longDurationRetryCfg
    , authTokenCacheExpiry = +600
    , registryUrl = common.registryUrl
    , gatewayUrl = gwUri
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , rideCfg = rideConfig
    , dashboardToken = sec.dashboardToken
    , cacheConfig
    , cacheTranslationConfig
    , maxEmergencyNumberCount = +3
    , minTripDistanceForReferralCfg = Some +1000
    }
