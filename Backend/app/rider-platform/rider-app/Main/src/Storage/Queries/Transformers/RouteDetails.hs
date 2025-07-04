module Storage.Queries.Transformers.RouteDetails where

import Data.List (sortOn)
import qualified Domain.Types.JourneyRouteDetails as JRD
import qualified Domain.Types.RouteDetails as RD
import Kernel.External.Maps.Google.MapsClient.Types
import Kernel.External.MultiModal.Interface.Types
import Kernel.Prelude
import Lib.JourneyLeg.Types

getTransformedRouteDetails :: [RD.RouteDetails] -> [MultiModalRouteDetails]
getTransformedRouteDetails routeDetails = do
  let sortedRouteDetails = sortOn (.subLegOrder) routeDetails
  map transformRouteDetails sortedRouteDetails
  where
    transformRouteDetails :: RD.RouteDetails -> MultiModalRouteDetails
    transformRouteDetails rd =
      MultiModalRouteDetails
        { gtfsId = RD.routeGtfsId rd,
          longName = RD.routeLongName rd,
          shortName = RD.routeShortName rd,
          color = RD.routeColorCode rd,
          alternateShortNames = fromMaybe [] (RD.alternateShortNames rd),
          fromStopDetails =
            Just $
              MultiModalStopDetails
                { stopCode = RD.fromStopCode rd,
                  name = RD.fromStopName rd,
                  gtfsId = RD.fromStopGtfsId rd,
                  platformCode = RD.fromStopPlatformCode rd
                },
          toStopDetails =
            Just $
              MultiModalStopDetails
                { stopCode = RD.toStopCode rd,
                  name = RD.toStopName rd,
                  gtfsId = RD.toStopGtfsId rd,
                  platformCode = RD.toStopPlatformCode rd
                },
          startLocation =
            LocationV2
              { latLng =
                  LatLngV2
                    { latitude = fromMaybe (error "Missing start location latitude") (RD.startLocationLat rd),
                      longitude = fromMaybe (error "Missing start location latitude") (RD.startLocationLon rd)
                    }
              },
          endLocation =
            LocationV2
              { latLng =
                  LatLngV2
                    { latitude = fromMaybe (error "Missing start location latitude") (RD.endLocationLat rd),
                      longitude = fromMaybe (error "Missing start location latitude") (RD.endLocationLon rd)
                    }
              },
          subLegOrder = fromMaybe 1 (RD.subLegOrder rd),
          fromArrivalTime = RD.fromArrivalTime rd,
          fromDepartureTime = RD.fromDepartureTime rd,
          toArrivalTime = RD.toArrivalTime rd,
          toDepartureTime = RD.toDepartureTime rd
        }

getTransformedJourneyRouteDetails :: [JRD.JourneyRouteDetails] -> [MultiModalJourneyRouteDetails]
getTransformedJourneyRouteDetails routeDetails = do
  let sortedRouteDetails = sortOn (\rd -> rd.subLegOrder) routeDetails
  map transformJourneyRouteDetails sortedRouteDetails
  where
    transformJourneyRouteDetails :: JRD.JourneyRouteDetails -> MultiModalJourneyRouteDetails
    transformJourneyRouteDetails rd =
      MultiModalJourneyRouteDetails
        { platformNumber = JRD.platformNumber rd,
          lineColorCode = JRD.lineColorCode rd,
          lineColor = JRD.lineColor rd,
          alternateShortNames = fromMaybe [] (JRD.alternateShortNames rd),
          journeyStatus = JRD.journeyStatus rd,
          subLegOrder = JRD.subLegOrder rd,
          frequency = JRD.frequency rd,
          routeLongName = JRD.routeLongName rd,
          fromStationId = JRD.fromStationId rd,
          toStationId = JRD.toStationId rd,
          routeId = JRD.routeId rd
        }

getTransformedJourneyRouteDetailsT :: [JRD.JourneyRouteDetails] -> [MultiModalJourneyRouteDetails]
getTransformedJourneyRouteDetailsT routeDetails = do
  let sortedRouteDetails = sortOn (\rd -> rd.subLegOrder) routeDetails
  map transformJourneyRouteDetails sortedRouteDetails
  where
    transformJourneyRouteDetails :: JRD.JourneyRouteDetails -> MultiModalJourneyRouteDetails
    transformJourneyRouteDetails rd =
      MultiModalJourneyRouteDetails
        { platformNumber = JRD.platformNumber rd,
          lineColorCode = JRD.lineColorCode rd,
          lineColor = JRD.lineColor rd,
          alternateShortNames = fromMaybe [] (JRD.alternateShortNames rd),
          journeyStatus = JRD.journeyStatus rd,
          subLegOrder = JRD.subLegOrder rd,
          frequency = JRD.frequency rd,
          routeLongName = JRD.routeLongName rd,
          fromStationId = JRD.fromStationId rd,
          toStationId = JRD.toStationId rd,
          routeId = JRD.routeId rd
        }
