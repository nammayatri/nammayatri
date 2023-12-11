let common = ./common.dhall

let useRealGoogle = Some common.googleCfg

let doNotUseRealGoogle =
      None { googleMapsUrl : Text, googleRoadsUrl : Text, googleKey : Text }

let criticalAPIs =
      { criticalAPIList =
        [ "/v2/maps/getPlacename/"
        , "/v2/maps/getPlaceDetails/"
        , "/v2/maps/autocomplete/"
        ]
      }

in  { port = +8019
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-google.log" }
    , graceTerminationPeriod = +90
    , mockDataPath = "./app/mocks/google/mock-data/"
    , googleCfg = doNotUseRealGoogle
    , snapToRoadIdentityMode = False
    , criticalAPIs
    }
