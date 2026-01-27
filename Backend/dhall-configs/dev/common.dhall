let sec = ./secrets/common.dhall

let globalCommon = ../generic/common.dhall

let nsdlRegistryUrl = "https://pilot-gateway-1.beckn.nsdl.co.in/"

let becknOneRegistryUrl = "https://beckn-one.succinct.in/subscribers"

let googleRouteConfig =
      { computeAlternativeRoutes = False
      , routePreference = "TRAFFIC_AWARE_OPTIMAL"
      , url = "https://routes.googleapis.com/"
      }

let googleCfg =
      { googleMapsUrl = "https://maps.googleapis.com/maps/api/"
      , googleRoadsUrl = "https://roads.googleapis.com/"
      , googleKey = sec.googleKey
      , useAdvancedDirections = True
      , googlePlaceNewUrl = "https://places.googleapis.com/v1/"
      , useNewPlaces = True
      , googleRouteConfig
      }

let mockGoogleCfg =
      { googleMapsUrl = "http://localhost:8019/"
      , googleRoadsUrl = "http://localhost:8019/"
      , googleKey = "mock-google-key"
      , useAdvancedDirections = True
      , googlePlaceNewUrl = "http://localhost:8019/"
      , useNewPlaces = True
      , googleRouteConfig
      }

let internalEndPointMap =
      [ { mapKey = "http://localhost:8015/v1"
        , mapValue = "http://localhost:8015/v1"
        }
      , { mapKey = "http://localhost:3000", mapValue = "http://localhost:3000" }
      ]

let sosAlertsTopicARN =
      "arn:aws:chatbot::463356420488:chat-configuration/slack-channel/sos-notifications"

let GCPProjectId = "ny-sandbox"

let GCPTopicId = "slack-alerts"

let sendGridUrl = "https://api.sendgrid.com/v3/mail/send"

let emailServiceConfig = { sendGridUrl = Some sendGridUrl }

let slackNotificationConfig =
      { snsTopicArn = Some sosAlertsTopicARN
      , gcpProjectId = Some GCPProjectId
      , gcpTopicId = Some GCPTopicId
      }

let SchedulerType = < RedisBased | DbBased >

let urlShortnerConfig =
      { url = "http://localhost:9023/", apiKey = sec.urlShortnerApiKey }

let ondcRegistryUrl = "http://localhost:8020/"

let ondcGatewayUrl = "http://localhost:8015/v1"

let nyRegistryUrl = "http://localhost:8020/"

let nyGatewayUrl = "http://localhost:8015/v1"

let nammayatriRegistryConfig =
      { apiKey = sec.nammayatriRegistryApiKey, url = nyRegistryUrl }

in  { smsSessionConfig = globalCommon.smsSessionConfig
    , autoMigrate = globalCommon.autoMigrate
    , loggerConfig =
            globalCommon.loggerConfig
        //  { logToFile = True, logRawSql = True, prettyPrinting = True }
    , LogLevel = globalCommon.LogLevel
    , kafkaCompression = globalCommon.kafkaCompression
    , s3Config = sec.s3Config
    , s3PublicConfig = sec.s3PublicConfig
    , slackToken = sec.slackToken
    , signatureExpiry = globalCommon.signatureExpiry
    , httpClientOptions = globalCommon.httpClientOptions
    , shortDurationRetryCfg = globalCommon.shortDurationRetryCfg
    , longDurationRetryCfg = globalCommon.longDurationRetryCfg
    , ServerName = globalCommon.ServerName
    , periodType = globalCommon.periodType
    , smsUserName = sec.smsUserName
    , smsPassword = sec.smsPassword
    , InfoBIPConfig = sec.InfoBIPConfig
    , passetto = { _1 = "localhost", _2 = 8079 }
    , googleCfg
    , mockGoogleCfg
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , nammayatriRegistryConfig
    , authServiceUrl = "http://localhost:8013/"
    , consumerType = globalCommon.consumerType
    , schedulerType = SchedulerType
    , internalEndPointMap
    , urlShortnerConfig
    , sosAlertsTopicARN
    , ondcRegistryUrl
    , ondcGatewayUrl
    , nyRegistryUrl
    , nyGatewayUrl
    , emailServiceConfig
    , slackNotificationConfig
    }
