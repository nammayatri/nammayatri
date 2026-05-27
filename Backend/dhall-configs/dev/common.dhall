let sec = ./secrets/common.dhall

let globalCommon = ../generic/common.dhall

let riderAppPort = Natural/show (env:RIDER_APP_PORT ? 8013)

let mockGooglePort = Natural/show (env:MOCK_GOOGLE_PORT ? 8019)

let becknGatewayPort = Natural/show (env:BECKN_GATEWAY_PORT ? 8015)

let mockRegistryPort = Natural/show (env:MOCK_REGISTRY_PORT ? 8020)

let uiDevPort = Natural/show (env:UI_DEV_PORT ? 3000)

let urlShortnerPort = Natural/show (env:URL_SHORTNER_PORT ? 9023)

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
      { googleMapsUrl = "http://localhost:${mockGooglePort}/"
      , googleRoadsUrl = "http://localhost:${mockGooglePort}/"
      , googleKey = "mock-google-key"
      , useAdvancedDirections = True
      , googlePlaceNewUrl = "http://localhost:${mockGooglePort}/"
      , useNewPlaces = True
      , googleRouteConfig
      }

let internalEndPointMap =
      [ { mapKey = "http://localhost:${becknGatewayPort}/v1"
        , mapValue = "http://localhost:${becknGatewayPort}/v1"
        }
      , { mapKey = "http://localhost:${uiDevPort}"
        , mapValue = "http://localhost:${uiDevPort}"
        }
      ]

let SchedulerType = < RedisBased | DbBased >

let urlShortnerConfig =
      { url = "http://localhost:${urlShortnerPort}/"
      , apiKey = sec.urlShortnerApiKey
      }

let ondcRegistryUrl = "http://localhost:${mockRegistryPort}/"

let ondcGatewayUrl = "http://localhost:${becknGatewayPort}/v1"

let nyRegistryUrl = "http://localhost:${mockRegistryPort}/"

let nyGatewayUrl = "http://localhost:${becknGatewayPort}/v1"

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
    , passetto = { _1 = "localhost", _2 = env:PASSETTO_SERVICE_PORT ? 8021 }
    , googleCfg
    , mockGoogleCfg
    , googleTranslateUrl = "https://www.googleapis.com/"
    , googleTranslateKey = sec.googleTranslateKey
    , nammayatriRegistryConfig
    , authServiceUrl = "http://localhost:${riderAppPort}/"
    , consumerType = globalCommon.consumerType
    , schedulerType = SchedulerType
    , internalEndPointMap
    , urlShortnerConfig
    , ondcRegistryUrl
    , ondcGatewayUrl
    , nyRegistryUrl
    , nyGatewayUrl
    }
