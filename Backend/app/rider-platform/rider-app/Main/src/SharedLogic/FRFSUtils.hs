{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.FRFSUtils where

import qualified API.Types.UI.FRFSTicketService as APITypes
import qualified BecknV2.FRFS.Enums as Spec
import Data.Aeson as A
import Data.List (groupBy, nub)
import Domain.Types.AadhaarVerification as DAadhaarVerification
import qualified Domain.Types.FRFSConfig as Config
import qualified Domain.Types.FRFSTicket as DT
import qualified Domain.Types.FRFSTicketBookingPayment as DTBP
import Domain.Types.FRFSTicketDiscount as DFRFSTicketDiscount
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.PartnerOrganization as DPO
import qualified Domain.Types.Person as DP
import qualified Domain.Types.Route as Route
import qualified Domain.Types.Station as Station
import EulerHS.Prelude ((+||), (||+))
import Kernel.Beam.Functions as B
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config (EsqDBReplicaFlow)
import Kernel.Types.Id
import qualified Kernel.Types.TimeBound as DTB
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as LYTU
import qualified Lib.Yudhishthira.Types as LYT
import Storage.Beam.Yudhishthira ()
import qualified Storage.CachedQueries.PartnerOrgStation as CQPOS
import qualified Storage.CachedQueries.Station as CQS
import Storage.Queries.AadhaarVerification as QAV
import Storage.Queries.FRFSTicketDiscount as QFRFSTicketDiscount
import Storage.Queries.Route as QRoute
import Storage.Queries.RouteStopMapping as QRouteStopMapping
import Tools.DynamicLogic
import Tools.Error

mkTicketAPI :: DT.FRFSTicket -> APITypes.FRFSTicketAPI
mkTicketAPI DT.FRFSTicket {..} = APITypes.FRFSTicketAPI {..}

mkPOrgStationAPIRes :: (CacheFlow m r, EsqDBFlow m r) => Station.Station -> Maybe (Id DPO.PartnerOrganization) -> m APITypes.FRFSStationAPI
mkPOrgStationAPIRes Station.Station {..} mbPOrgId = do
  pOrgStation <- mbPOrgId `forM` \pOrgId -> B.runInReplica $ CQPOS.findByStationIdAndPOrgId id pOrgId >>= fromMaybeM (PartnerOrgStationNotFoundForStationId pOrgId.getId id.getId)
  let pOrgStationName = pOrgStation <&> (.name)
  pure $ APITypes.FRFSStationAPI {name = fromMaybe name pOrgStationName, stationType = Nothing, color = Nothing, sequenceNum = Nothing, distance = Nothing, towards = Nothing, ..}

mkTBPStatusAPI :: DTBP.FRFSTicketBookingPaymentStatus -> APITypes.FRFSBookingPaymentStatusAPI
mkTBPStatusAPI = \case
  DTBP.PENDING -> APITypes.PENDING
  DTBP.SUCCESS -> APITypes.SUCCESS
  DTBP.FAILED -> APITypes.FAILURE
  DTBP.REFUND_PENDING -> APITypes.REFUND_PENDING
  DTBP.REFUNDED -> APITypes.REFUNDED

mkFRFSConfigAPI :: Config.FRFSConfig -> APITypes.FRFSConfigAPIRes
mkFRFSConfigAPI Config.FRFSConfig {..} = do
  APITypes.FRFSConfigAPIRes {isEventOngoing = False, ticketsBookedInEvent = 0, ..}

mkPOrgStationAPI :: (CacheFlow m r, EsqDBFlow m r) => Maybe (Id DPO.PartnerOrganization) -> APITypes.FRFSStationAPI -> m APITypes.FRFSStationAPI
mkPOrgStationAPI mbPOrgId stationAPI = do
  station <- B.runInReplica $ CQS.findByStationCode stationAPI.code >>= fromMaybeM (StationNotFound $ "station code:" +|| stationAPI.code ||+ "")
  mkPOrgStationAPIRes station mbPOrgId

data FRFSTicketDiscountDynamic = FRFSTicketDiscountDynamic
  { aadhaarData :: Maybe DAadhaarVerification.AadhaarVerification,
    discounts :: [DFRFSTicketDiscount.FRFSTicketDiscount]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

getFRFSTicketDiscountWithEligibility ::
  ( MonadFlow m,
    CacheFlow m r,
    EsqDBFlow m r,
    EsqDBReplicaFlow m r
  ) =>
  Id DM.Merchant ->
  Id DMOC.MerchantOperatingCity ->
  Spec.VehicleCategory ->
  Id DP.Person ->
  [Id FRFSTicketDiscount] ->
  m [(FRFSTicketDiscount, Bool)]
getFRFSTicketDiscountWithEligibility merchantId merchantOperatingCityId vehicleType personId applicableDiscountIds = do
  availableDiscounts <-
    pure . catMaybes
      =<< mapM
        ( \applicableDiscountId -> QFRFSTicketDiscount.findByIdAndVehicleAndCity applicableDiscountId vehicleType merchantId merchantOperatingCityId
        )
        applicableDiscountIds
  aadhaarVerification <- QAV.findByPersonId personId
  applicableDiscounts <- do
    let ticketDiscountData = FRFSTicketDiscountDynamic {aadhaarData = aadhaarVerification, discounts = availableDiscounts}
    localTime <- getLocalCurrentTime 19800 -- Fix Me
    (allLogics, _) <- getAppDynamicLogic (cast merchantOperatingCityId) LYT.FRFS_DISCOUNTS localTime Nothing
    response <- try @_ @SomeException $ LYTU.runLogics allLogics ticketDiscountData
    case response of
      Left e -> do
        logError $ "Error in running FRFS Discount Logic - " <> show e <> " - " <> show ticketDiscountData <> " - " <> show allLogics
        return []
      Right resp ->
        case (A.fromJSON resp.result :: Result FRFSTicketDiscountDynamic) of
          A.Success result -> return result.discounts
          A.Error err -> do
            logError $ "Error in parsing FRFSTicketDiscountDynamic - " <> show err <> " - " <> show resp <> " - " <> show ticketDiscountData <> " - " <> show allLogics
            return []
  return $ mergeDiscounts availableDiscounts applicableDiscounts
  where
    mergeDiscounts availableDiscounts applicableDiscounts =
      map (\discount -> (discount, discount `elem` applicableDiscounts)) availableDiscounts

data RouteStopInfo = RouteStopInfo
  { route :: Route.Route,
    startStopCode :: Text,
    endStopCode :: Text,
    totalStops :: Maybe Int,
    travelTime :: Seconds
  }

getPossibleRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m [RouteStopInfo]
getPossibleRoutesBetweenTwoStops startStationCode endStationCode = do
  routesWithStop <- B.runInReplica $ QRouteStopMapping.findByStopCode startStationCode
  let routeCodes = nub $ map (.routeCode) routesWithStop
  routeStops <- B.runInReplica $ QRouteStopMapping.findByRouteCodes routeCodes
  currentTime <- getCurrentTime
  let serviceableStops = DTB.findBoundedDomain routeStops currentTime ++ filter (\stop -> stop.timeBounds == DTB.Unbounded) routeStops
      groupedStops = groupBy (\a b -> a.routeCode == b.routeCode) serviceableStops
      possibleRoutes =
        nub $
          catMaybes $
            map
              ( \stops ->
                  let mbStartStopSequence = (.sequenceNum) <$> find (\stop -> stop.stopCode == startStationCode) stops
                   in (\stop -> (stop.routeCode, (\startStopSequence -> stop.sequenceNum - startStopSequence) <$> mbStartStopSequence)) <$> find (\stop -> maybe False (\startStopSequence -> stop.stopCode == endStationCode && stop.sequenceNum > startStopSequence) mbStartStopSequence) stops
              )
              groupedStops
  routes <- QRoute.findByRouteCodes (map fst possibleRoutes)
  return $
    map
      ( \route ->
          RouteStopInfo
            { route,
              totalStops = snd =<< find (\(routeCode, _) -> routeCode == route.code) possibleRoutes,
              startStopCode = startStationCode,
              endStopCode = endStationCode,
              travelTime = Seconds 1800 -- TODO :: Fix With Average Tracking Speed and Distance
            }
      )
      routes

-- TODO :: This to be handled from OTP, Currently Hardcode for Chennai
getPossibleTransitRoutesBetweenTwoStops :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Text -> Text -> m [[RouteStopInfo]]
getPossibleTransitRoutesBetweenTwoStops startStationCode endStationCode = do
  case (startStationCode, endStationCode) of
    ("MBTcSIip", "jQaLNViL") -> do
      routes <- QRoute.findByRouteCodes ["jylLjHej", "BTuKbmBy"]
      return $
        [ map
            ( \route ->
                if route.code == "jylLjHej"
                  then
                    RouteStopInfo
                      { route,
                        totalStops = Just 6,
                        startStopCode = "MBTcSIip",
                        endStopCode = "TiulEaYs",
                        travelTime = Seconds 660 -- TODO :: Fix With Average Tracking Speed and Distance
                      }
                  else
                    RouteStopInfo
                      { route,
                        totalStops = Just 8,
                        startStopCode = "TiulEaYs",
                        endStopCode = "jQaLNViL",
                        travelTime = Seconds 1440 -- TODO :: Fix With Average Tracking Speed and Distance
                      }
            )
            routes
        ]
    _ -> return []

-- data VehicleInfo = VehicleInfo
--   { latitude :: Maybe Double,
--     longitude :: Maybe Double,
--     speed :: Maybe Text
--   }
--   deriving stock (Generic, Show)
--   deriving anyclass (ToJSON, FromJSON, ToSchema)

-- data MergedType = Bus | Stop | Route

-- data MergedPolyline = MergedPolyline
--   { point :: LatLong
--   , _type :: MergedType
--   }

-- getUpcomingStopsETAForNearestBus :: LatLong -> m [MergedPolyline]
-- getUpcomingStopsETAForNearestBus currentLocation = do
--   routeInfoFromRedis <- Redis.withCrossAppRedis $ Redis.hGetAll (makeRouteKey routeCode)

--   if not (null routeInfoFromRedis)
--     then do
--       -- Find the nearest vehicle
--       let nearestVehicle = minimumBy (\(_, vehInfo) -> distanceBetweenInMeters currentLocation (LatLong vehInfo.latitude vehInfo.longitude)) routeInfoFromRedis

--       -- Get route info
--       QRoute.findByRouteCode routeCode >>= \case
--         Just route -> do
--           case route.polyline of
--             Just polyline -> do
--               let waypoints = map (\point -> LatLong {lat = point.latitude, lon = point.longitude}) (decode polyline)
--               let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) reqStops

--               -- Merge the stops, bus location, and polyline points into a unified structure
--               let mergedPolyline = mergeStopsWithPolyline (LatLong nearestVehicle.latitude nearestVehicle.longitude) sortedStops waypoints

--               -- Calculate ETAs for the upcoming stops after the bus's current location
--               let etaForStops = findETAForUpcomingStops nearestVehicle.speed (LatLong nearestVehicle.latitude nearestVehicle.longitude) mergedPolyline

--               return etaForStops
--             Nothing -> throwError "Polyline missing"
--         Nothing -> throwError "Route not found"
--     else throwError "No vehicle data found"

-- -- Function to merge the bus location, stops, and polyline
-- mergeStopsWithPolyline :: LatLong -> [LatLong] -> [LatLong] -> [MergedPolyline]
-- mergeStopsWithPolyline busLocation stops polyline =
--     -- Merge all points (bus location, stops, and polyline) into a single list
--     sortBy comparePoints $ concat
--       [ [MergedPolyline busLocation Bus] -- Add bus location as a 'Bus' type point
--       , map (\stop -> MergedPolyline stop Stop) stops -- Map stops as 'Stop' type points
--       , map (\routePoint -> MergedPolyline routePoint Route) polyline -- Map polyline points as 'Route'
--       ]
--   where
--     -- Custom comparison function to maintain sequence or proximity order
--     comparePoints (MergedPolyline p1 _) (MergedPolyline p2 _) =
--       distanceFromStart p1 `compare` distanceFromStart p2

--     -- Compute a "distance from start" metric based on proximity to the starting point (you can customize this)
--     distanceFromStart :: LatLong -> Double
--     distanceFromStart point = -- Implement based on the specific route geometry
--       distanceBetweenInMeters startOfPolyline point -- For example, comparing each point with the start of the polyline

-- -- Function to compute ETAs for upcoming stops after the current bus location
-- findETAForUpcomingStops :: Maybe Text -> LatLong -> [MergedPolyline] -> [(LatLong, Double)]
-- findETAForUpcomingStops mSpeed currentLocation mergedPolyline =
--     let nextStops = filter (\p -> _type p == Stop && isAhead currentLocation (point p)) mergedPolyline
--         currentSpeed = maybe 10 (read . unpack) mSpeed -- Default speed 10 km/h if not available
--     in map (\stop -> (point stop, calculateETA currentLocation (point stop) currentSpeed)) nextStops

-- -- Check if a stop is ahead of the current location based on route direction
-- isAhead :: LatLong -> LatLong -> Bool
-- isAhead current stop = -- Implement this based on the route movement direction
--   -- For simplicity, assume any stop with a greater sequence number is ahead
--   distanceBetweenInMeters current stop > 0

-- -- Function to calculate ETA based on distance and speed
-- calculateETA :: LatLong -> LatLong -> Double -> Double
-- calculateETA from to speed =
--   let distance = distanceBetweenInMeters from to
--   in distance / (speed * 1000 / 60) -- Convert speed (km/h) to minutes per km

-- -- Helper function to calculate distance between two LatLong points in meters
-- distanceBetweenInMeters :: LatLong -> LatLong -> Double
-- distanceBetweenInMeters = -- Implement Haversine formula or use an existing library
--   haversineDistance -- Pseudocode, replace with your distance calculation logic
