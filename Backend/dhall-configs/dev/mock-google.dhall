let common = ./common.dhall

let useRealGoogle = Some common.googleCfg

let RoutingPreference =
      < ROUTING_PREFERENCE_UNSPECIFIED
      | TRAFFIC_UNAWARE
      | TRAFFIC_AWARE
      | TRAFFIC_AWARE_OPTIMAL
      >

let googleRouteConfigType =
      { computeAlternativeRoutes : Bool
      , routePreference : RoutingPreference
      , url : Text
      , useRouteMatrix : Optional Bool
      }

let doNotUseRealGoogle =
      None
        { googleMapsUrl : Text
        , googleRoadsUrl : Text
        , googleKey : Text
        , useAdvancedDirections : Bool
        , googleRouteConfig : googleRouteConfigType
        , googlePlaceNewUrl : Text
        , useNewPlaces : Bool
        , googleAutocompleteParams : Optional (List Text)
        , mobilityBillingUrl : Optional Text
        }

in  { port = Natural/toInteger (env:SERVICE_PORT ? 8019)
    , loggerConfig =
        common.loggerConfig // { logFilePath = "/tmp/mock-google.log" }
    , graceTerminationPeriod = +90
    , mockDataPath = "./app/mocks/google/mock-data/"
    , googleCfg = doNotUseRealGoogle
    , snapToRoadIdentityMode = False
    }
