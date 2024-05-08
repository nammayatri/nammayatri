let ondclogsUrl = "https://logs.ondc.in"

let ondcTokenMap
    : List
        { mapKey : { merchantOperatingCityId : Text, domain : Text }
        , mapValue : { token : Text, ondcUrl : Text }
        }
    = [ { mapKey =
          { merchantOperatingCityId = "NAMMA_YATRI_BANGALORE"
          , domain = "MOBILITY"
          }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey =
          { merchantOperatingCityId = "YATRI_KOCHI", domain = "MOBILITY" }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey =
          { merchantOperatingCityId = "YATRI_SATHI_KOLKATA"
          , domain = "MOBILITY"
          }
        , mapValue = { token = "abcd123", ondcUrl = ondclogsUrl }
        }
      , { mapKey =
          { merchantOperatingCityId = "NAMMA_YATRI_CHENNAI"
          , domain = "PUBLIC_TRANSPORT"
          }
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
