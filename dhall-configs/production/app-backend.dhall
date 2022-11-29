let common = ./common.dhall

let sec = ./secrets/app-backend.dhall

let esqDBCfg =
      { connectHost = "adb.primary.beckn.juspay.net"
      , connectPort = 5432
      , connectUser = sec.dbUserId
      , connectPassword = sec.dbPassword
      , connectDatabase = "atlas_app_pilot"
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

let hcfg =
      { connectHost = "cache.primary.beckn.juspay.net"
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
      , useFakeSms = None Natural
      , url = "https://http.myvfirst.com"
      , sender = "JUSPAY"
      }

let gwUri = "https://api.beckn.juspay.in/gateway/v1"

let InfoBIPConfig =
      { username = common.InfoBIPConfig.username
      , password = common.InfoBIPConfig.password
      , token = common.InfoBIPConfig.token
      , url = "https://5vmxvj.api.infobip.com"
      , webhookurl = "https://5vmxvj.api.infobip.com"
      , sender = "JUSPAY"
      }

let WebengageConfig = { url = "https://st.in.webengage.com" }

let nsdlGwUri = "https://gateway-1.beckn.nsdl.co.in"

let apiRateLimitOptions = { limit = +4, limitResetTimeInSec = +600 }

let searchRateLimitOptions = { limit = +10, limitResetTimeInSec = +60 }

let slackCfg =
      { channelName = "#beckn-driver-onboard-test"
      , slackToken = common.slackToken
      }

let httpClientOptions = { timeoutMs = +10000, maxRetries = +3 }

let encTools = { service = common.passetto, hashSalt = sec.encHashSalt }

let kafkaProducerCfg = { brokers = [] : List Text }

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
    , otpSmsTemplate = "<#> Your OTP for login to Yatri App is {#otp#} {#hash#}"
    , port = +8013
    , metricsPort = +9999
    , hostName = "juspay.in"
    , selfUIUrl = "https://api.beckn.juspay.in/pilot/bap/v2/"
    , bapSelfIds =
      { cabs = "api.beckn.juspay.in/pilot/bap/cab/v1"
      , metro = "api.beckn.juspay.in/pilot/bap/metro/v1"
      }
    , bapSelfURIs =
      { cabs = "https://api.beckn.juspay.in/pilot/bap/cab/v1"
      , metro = "https://api.beckn.juspay.in/pilot/bap/metro/v1"
      }
    , bapSelfUniqueKeyIds = { cabs = "3", metro = "4" }
    , signingKey = sec.signingKey
    , signatureExpiry = common.signatureExpiry
    , searchRequestExpiry = Some +600
    , exotelCfg = Some common.exotelCfg
    , migrationPath = None Text
    , autoMigrate = common.autoMigrate
    , coreVersion = "0.9.3"
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/app-backend.log" }
    , googleTranslateUrl = common.googleTranslateUrl
    , googleTranslateKey = common.googleTranslateKey
    , metricsSearchDurationTimeout = +45
    , graceTerminationPeriod = +90
    , apiRateLimitOptions
    , searchRateLimitOptions
    , slackCfg
    , searchLimitExceedNotificationTemplate =
        "Customer with {#cust-id#} is exceeding the search limit."
    , httpClientOptions
    , authTokenCacheExpiry = +600
    , registryUrl = common.registryUrl
    , gatewayUrl = gwUri
    , disableSignatureAuth = False
    , encTools
    , kafkaProducerCfg
    , rideCfg = rideConfig
    , cacheConfig
    , dashboardToken = sec.dashboardToken
    , cacheTranslationConfig
    }
