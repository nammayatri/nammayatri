let ondclogsUrl = "https://logs.ondc.in"

let googleSAPrivateKey = "some-secret-private-key-for-google-service-account"

let ondcTokenMap
    : List
        { mapKey : { merchantId : Text, domain : Text }
        , mapValue : { token : Text, ondcUrl : Text }
        }
    = [ { mapKey = { merchantId = "NAMMA_YATRI", domain = "MOBILITY" }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey = { merchantId = "YATRI", domain = "MOBILITY" }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey = { merchantId = "YATRI_SATHI", domain = "MOBILITY" }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey = { merchantId = "NAMMA_YATRI", domain = "PUBLIC_TRANSPORT" }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      ]

in  { dbUserId = "atlas_app_user"
    , dbPassword = "atlas"
    , smsOtpHash = "xxxxxxx"
    , signingKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="
    , encHashSalt =
        "How wonderful it is that nobody need wait a single moment before starting to improve the world"
    , dashboardToken = "some-secret-dashboard-token-for-rider-app"
    , internalAPIKey = "test-bap-api-key"
    , internalClickhouseAPIKey = "test-clickhosue-api-key"
    , clickHouseUsername = "default"
    , clickHousePassword = ""
    , locationTrackingServiceKey = "ae288466-2add-11ee-be56-0242ac120002"
    , ondcTokenMap
    , googleSAPrivateKey
    , parkingApiKey = "test-parking-api-key"
    }
