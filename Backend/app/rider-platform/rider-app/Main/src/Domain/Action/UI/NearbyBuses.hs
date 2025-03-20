{-# OPTIONS_GHC -Wwarn=unused-imports #-}

module Domain.Action.UI.NearbyBuses (postNearbyBusBooking) where

import qualified API.Types.UI.NearbyBuses
import qualified BecknV2.FRFS.Enums
import Data.Text.Encoding (decodeUtf8)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.RecentLocation
import qualified Domain.Types.VehicleRouteMapping as DTVRM
import qualified Environment
import EulerHS.Prelude hiding (decodeUtf8, id)
import qualified Kernel.External.Maps.Types as Maps
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import qualified Storage.Queries.FRFSTicketBooking as QFRFSTicketBooking
import qualified Storage.Queries.RecentLocation as QRecentLocation
import qualified Storage.Queries.VehicleRouteMapping as QVehicleRouteMapping

nearbyBusKey :: Text
nearbyBusKey = "bus_locations"

postNearbyBusBooking ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    API.Types.UI.NearbyBuses.NearbyBusesRequest ->
    Environment.Flow API.Types.UI.NearbyBuses.NearbyBusesResponse
  )
postNearbyBusBooking (_, _) req = do
  let radius :: Double = 0.5 --TODO: To be moved to config.

  -- Convert ByteString to Text after geo search
  busesBS :: [ByteString] <- Hedis.withCrossAppRedis $ Hedis.geoSearch nearbyBusKey (Hedis.FromLonLat req.userLat req.userLon) (Hedis.ByRadius radius "km")
  let buses = map decodeUtf8 busesBS

  busRouteMapping <- QVehicleRouteMapping.findAllByVehicleNumber buses
  let routIds :: [Text] = map DTVRM.routeId busRouteMapping

  recentLocations <- QRecentLocation.findRecentLocationsByRouteIds req.personId routIds

  -- Process recent locations to build RecentRide objects
  recentRidesNested <- forM recentLocations $ \recentLoc -> do
    case recentLoc.entityType of
      Domain.Types.RecentLocation.BUS -> do
        let limit = 10
            journeyId = recentLoc.entityId -- Assuming entityId is the journeyId

        -- Fix the function call with correct parameters
        frfstickets <- QFRFSTicketBooking.findAllByJourneyIdAndRiderId req.personId journeyId limit

        -- Process each ticket
        rides <- forM frfstickets $ \ticket -> do
          case ticket.vehicleType of
            BecknV2.FRFS.Enums.BUS ->
              -- Create RecentRide, handling Maybe routeId properly
              return $
                Just $
                  API.Types.UI.NearbyBuses.RecentRide
                    (getId ticket.fromStationId)
                    ticket.quantity
                    recentLoc.routeId -- This is already Maybe Text so no need to wrap
                    (getId ticket.toStationId)
            _ -> return Nothing
        return $ catMaybes rides
      _ -> return []

  -- Flatten the nested list and prepare the response
  let recentRides = concat recentRidesNested

  -- Create nearby bus objects
  let nearbyBuses =
        map
          ( \bus ->
              API.Types.UI.NearbyBuses.NearbyBus
                { busId = Just bus.vehicleNo,
                  routeId = bus.routeId,
                  currentLocation = Maps.LatLong 0.0 0.0, -- Placeholder coordinates
                  nextStop = Just "nextStop", -- Placeholder stop name
                  distance = Just 0.0, -- Placeholder distance
                  eta = Just 10, -- Placeholder ETA in minutes
                  capacity = Just 10, -- Placeholder capacity
                  occupancy = Just 0 -- Placeholder occupancy
                }
          )
          busRouteMapping

  -- Return the complete response
  return $ API.Types.UI.NearbyBuses.NearbyBusesResponse nearbyBuses recentRides
