{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ScheduledTrips (postScheduledTrips) where

import qualified API.Types.UI.ScheduledTrips as ScheduledTrips
-- import Data.List.NonEmpty as NE

import Data.Function
import qualified Data.List as List
import qualified Data.List.NonEmpty as NE
import Data.Maybe
import Data.OpenApi (ToSchema)
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

postScheduledTrips ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Route.Route ->
    ScheduledTrips.CurrLocation ->
    Environment.Flow ScheduledTrips.TrackingResp
    -- Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postScheduledTrips (mbPersonId, merchantId) routeId currLocation = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  reqRoute <- QRoute.findByRouteId routeId >>= fromMaybeM (InvalidRequest "Route not found")
  reqStops <- QRSM.findByRouteCode reqRoute.code
  let sortedStops = sortBy (compare `on` RouteStopMapping.sequenceNum) reqStops

  let stopPairs = map (\(x, y) -> (x, y)) (pairWithNext sortedStops)

  minDistancesWithPoint <-
    forM stopPairs $ \stopPair -> do
      let firstStop = fst stopPair
      let secondStop = snd stopPair
      let request =
            Maps.GetRoutesReq
              { waypoints = NE.fromList [firstStop.stopPoint, secondStop.stopPoint],
                calcPoints = True,
                mode = Just Maps.CAR
              }

      routeInfo <- Maps.getRoutes Nothing personId merchantId Nothing request
      reqRouteInfo <- (listToMaybe routeInfo) & fromMaybeM (InvalidRequest "Route not found")
      let wayPoints = reqRouteInfo.points
      distancesFromCurrLocation <-
        forM wayPoints $ \point -> do
          pure (highPrecMetersToMeters $ distanceBetweenInMeters (mkLatLong currLocation.lat currLocation.lon) point)
      let minDistance = minimum (pure distancesFromCurrLocation)
      pure (minDistance, secondStop)

  let nextStop = List.minimumBy (comparing fst) minDistancesWithPoint

  pure $ ScheduledTrips.TrackingResp {nextStop = snd nextStop}

mkLatLong :: Double -> Double -> Maps.LatLong
mkLatLong lat_ lon_ =
  Maps.LatLong
    { lat = lat_,
      lon = lon_
    }

pairWithNext :: [a] -> [(a, a)]
pairWithNext xs = zip xs (List.tail xs)
