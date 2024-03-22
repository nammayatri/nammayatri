let ondclogsUrl = "https://logs.ondc.in"

let ondcTokenMap
    : List { mapKey : Text, mapValue : { token : Text, ondcUrl : Text } }
    = [ { mapKey = "NAMMA_YATRI"
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey = "YATRI"
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey = "YATRI_SATHI"
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
    , clickHouseUsername = "default"
    , clickHousePassword = ""
    , ondcTokenMap
    }
