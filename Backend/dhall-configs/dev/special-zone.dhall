let sec = ./secrets/provider-dashboard.dhall

let esqDBCfg =
      { connectHost = "localhost"
      , connectPort = 5434
      , connectUser = "atlas_special_zone_user"
      , connectPassword = "atlas"
      , connectDatabase = "atlas_dev"
      , connectSchemaName = "atlas_special_zone"
      , connectionPoolCount = +25
      }

let esqDBReplicaCfg =
      { connectHost = esqDBCfg.connectHost
      , connectPort = 5435
      , connectUser = esqDBCfg.connectUser
      , connectPassword = esqDBCfg.connectPassword
      , connectDatabase = esqDBCfg.connectDatabase
      , connectSchemaName = esqDBCfg.connectSchemaName
      , connectionPoolCount = esqDBCfg.connectionPoolCount
      }

let LogLevel = < DEBUG | INFO | WARNING | ERROR >

let loggerConfig =
      { level = LogLevel.DEBUG
      , logToFile = True
      , logFilePath = "/tmp/special-location.log"
      , logToConsole = True
      , logRawSql = True
      , prettyPrinting = True
      }

let criticalAPIs =
      { criticalAPIList =
        [ "/v2/auth/"
        , "/v2/auth/signature/"
        , "/v2/auth/:authId/verify/"
        , "/v2/profile/"
        , "/v2/serviceability (/origin,/destination)"
        , "/v2/maps/getPlacename"
        , "/v2/maps /autocomplete"
        , "/v2/rideSearch/"
        , "/v2/rideSearch/:searchId/results/"
        , "/v2/estimate/:estimateId/results/"
        , "/v2/estimate/:estimateId/select2/"
        , "/v2/estimate/:estimateId/quotes/"
        , "/v2/estimate/:estimateId/cancel/"
        , "/v2/rideSearch/quotes/:quoteId/confirm/"
        , "/v2/rideBooking/:rideBookingId/"
        , "/v2/rideBooking/:rideBookingId/cancel/c"
        , "/v2/ride/:rideId/driver/location/"
        , "/beckn/cab/v1/:merchantId/on_search/"
        , "/beckn/cab/v1/:merchantId/on_select/"
        , "/beckn/cab/v1/:merchantId/on_init/"
        , "/beckn/cab/v1/:merchantId/on_confirm/"
        , "/beckn/cab/v1/:merchantId/on_track/"
        , "/beckn/cab/v1/:merchantId/on_status/"
        , "/v2/ride/:rideId/call/driver/"
        , "/v2/exotel/call/"
        , "/v2/sos/"
        ]
      }

in  { port = +8032
    , migrationPath = Some "dev/migrations/special-zone"
    , autoMigrate = True
    , esqDBCfg
    , esqDBReplicaCfg
    , dashboardToken = sec.specialZoneToken
    , loggerConfig
    , graceTerminationPeriod = +90
    , apiKey = "170f2a2a-b014-4a86-b4c0-e453e8v0b660"
    , criticalAPIs
    }
