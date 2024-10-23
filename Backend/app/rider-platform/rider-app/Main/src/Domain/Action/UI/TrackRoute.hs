{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.TrackRoute (getTrackVehicles) where

import qualified API.Types.UI.TrackRoute as TrackRoute
import Data.Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.OpenApi (ToSchema)
import qualified Data.Time
import qualified Data.Vector as V
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Domain.Types.RouteStopMapping as DRSM
import qualified Domain.Types.RouteStopMapping as RouteStopMapping
import qualified Environment
import qualified EulerHS.Language
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.APISuccess
import qualified Kernel.Types.APISuccess
import Kernel.Types.Distance
import qualified Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import Kernel.Utils.Common (fromMaybeM)
import Servant
import Storage.Queries.Route as QRoute
import qualified Storage.Queries.RouteStopMapping as QRSM
import Storage.Queries.Station as QStation
import Tools.Auth
import Tools.Error
import Tools.Maps as Maps

getTrackVehicles ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Prelude.Text ->
    Environment.Flow TrackRoute.TrackingResp
  )
getTrackVehicles (mbPersonId, merchantId) routeCode = do
  now <- getCurrentTime
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  routeInfoFromRedis :: [(Text, VehicleInfoForRoute)] <- Redis.hGetAll (makeRouteKey routeCode)
  reqStops <- QRSM.findByRouteCode routeCode
  let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) reqStops
      stopPairs = map (\(x, y) -> (x, y)) (pairWithNext sortedStops)

  stopPairsWithWaypoints <- do
    forM stopPairs $ \stopPair -> do
      let firstStop = fst stopPair
          secondStop = snd stopPair
          request =
            Maps.GetRoutesReq
              { waypoints = NE.fromList [firstStop.stopPoint, secondStop.stopPoint],
                calcPoints = True,
                mode = Just Maps.CAR
              }

      routeInfo <- Maps.getRoutes Nothing personId merchantId Nothing request
      reqRouteInfo <- (listToMaybe routeInfo) & fromMaybeM (InvalidRequest "Route not found")
      let wayPoints = reqRouteInfo.points
      pure (stopPair, wayPoints)

  trackingResp <-
    forM routeInfoFromRedis $ \singleRouteInfo -> do
      let vehicleId_ = fst singleRouteInfo
          vehicleInfo = snd singleRouteInfo
      vehicleLat <- vehicleInfo.latitude & fromMaybeM (InternalError "vehicle latitude not found")
      vehicleLong <- vehicleInfo.longitude & fromMaybeM (InternalError "vehicle longitude not found")

      minDistancesWithPoint <-
        forM stopPairsWithWaypoints $ \stopPairWithWaypoints -> do
          distancesFromCurrLocation <-
            forM (snd stopPairWithWaypoints) $ \point -> do
              pure (highPrecMetersToMeters $ distanceBetweenInMeters (mkLatLong vehicleLat vehicleLong) point)
          let minDistance = minimum (pure distancesFromCurrLocation)
          pure (minDistance, (snd (fst stopPairWithWaypoints)))

      let nextStop = List.minimumBy (comparing fst) minDistancesWithPoint

      pure $ TrackRoute.VehicleInfo {nextStop = snd nextStop, location = (mkLatLong vehicleLat vehicleLong), vehicleId = vehicleId_, locationUpdateTime = Just now}

  pure $ TrackRoute.TrackingResp {vehicleTrackingInfo = trackingResp}

mkLatLong :: Double -> Double -> Maps.LatLong
mkLatLong lat_ lon_ =
  Maps.LatLong
    { lat = lat_,
      lon = lon_
    }

pairWithNext :: [a] -> [(a, a)]
pairWithNext xs = zip xs (List.tail xs)

data VehicleInfoForRoute = VehicleInfoForRoute
  { startTime :: Maybe UTCTime,
    startDate :: Maybe Text,
    scheduleRelationship :: Maybe Text,
    tripId :: Maybe Text,
    latitude :: Maybe Double,
    longitude :: Maybe Double,
    speed :: Maybe Text,
    timestamp :: Maybe Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)

makeRouteKey :: Text -> Text
makeRouteKey routeId = "route:" <> routeId
