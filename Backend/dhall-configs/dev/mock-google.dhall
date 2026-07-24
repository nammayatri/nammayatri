let common = ./common.dhall

let useRealGoogle = Some common.googleCfg

let RoutingPreference =
      < ROUTING_PREFERENCE_UNSPECIFIED
      | TRAFFIC_UNAWARE
      | TRAFFIC_AWARE
      | TRAFFIC_AWARE_OPTIMAL
      >

let ExtraComputationV2 =
      < EXTRA_COMPUTATION_UNSPECIFIED
      | TRAFFIC_ON_POLYLINE
      | TOLLS
      | FUEL_CONSUMPTION
      | HTML_FORMATTED_NAVIGATION_INSTRUCTIONS
      | FLYOVER_INFO_ON_POLYLINE
      | NARROW_ROAD_INFO_ON_POLYLINE
      >

let googleRouteConfigType =
      { computeAlternativeRoutes : Bool
      , routePreference : RoutingPreference
      , extraComputations : Optional (List ExtraComputationV2)
      , url : Text
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
