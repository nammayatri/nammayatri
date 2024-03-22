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

in  { dbUserId = "atlas_driver_offer_bpp_user"
    , dbPassword = "atlas"
    , smsOtpHash = "xxxxxxx"
    , signingKey = "Lw9M+SHLY+yyTmqPVlbKxgvktZRfuIT8nHyE89Jmf+o="
    , encHashSalt =
        "How wonderful it is that nobody need wait a single moment before starting to improve the world"
    , dashboardToken = "some-secret-dashboard-token-for-driver-offer-bpp"
    , clickHouseUsername = "default"
    , clickHousePassword = ""
    , locDBUserId = "atlas_person_location_user"
    , locationTrackingServiceKey = "ae288466-2add-11ee-be56-0242ac120002"
    , appBackendApikey = "test-bap-api-key"
    , internalKey = "test-bpp-api-key"
    , ondcTokenMap
    }
