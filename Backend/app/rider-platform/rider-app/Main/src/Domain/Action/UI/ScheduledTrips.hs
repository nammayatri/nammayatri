{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Action.UI.ScheduledTrips (postScheduledTrips) where

import qualified API.Types.UI.ScheduledTrips
-- import qualified Kernel.External.Maps.Types as Maps

import qualified Data.List.NonEmpty as NE
import Data.OpenApi (ToSchema)
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person
import qualified Domain.Types.Route
import qualified Domain.Types.RouteStopMapping as DRSM
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
import Tools.Auth
import Tools.Error
import Tools.Maps as Maps

postScheduledTrips ::
  ( ( Kernel.Prelude.Maybe (Kernel.Types.Id.Id Domain.Types.Person.Person),
      Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
    ) ->
    Kernel.Types.Id.Id Domain.Types.Route.Route ->
    API.Types.UI.ScheduledTrips.CurrLocation ->
    Environment.Flow Kernel.Types.APISuccess.APISuccess
  )
postScheduledTrips (mbPersonId, merchantId) _routeId _currLocation = do
  personId <- mbPersonId & fromMaybeM (InvalidRequest "Person not found")
  -- reqRoute <- QRoute.findByRouteId routeId >>= fromMaybeM (InvalidRequest "Route not found")
  reqStops <- QRSM.findByRouteCode "SHUTTLE-U" --reqRoute.code
  stopLatLongs <-
    forM reqStops $ \stop -> do
      pure $ stop.stopPoint

  logDebug $ "hello world Stop Lat Longs ----------------------> " <> show stopLatLongs

  let request =
        Maps.GetRoutesReq
          { waypoints = NE.fromList stopLatLongs,
            calcPoints = True,
            mode = Just Maps.CAR
          }

  _routeInfo <- Maps.getRoutes Nothing personId merchantId Nothing request

  -- mapM_
  --   (\route -> do

  --   ) _routeInfo

  logDebug $ "hello world Route Info ----------------------> " <> show _routeInfo

  pure Kernel.Types.APISuccess.Success

-- mkLatLong :: Double -> Double -> Maps.LatLong
-- mkLatLong lat_ lon_ =
--   Maps.LatLong
--     { lat = lat_,
--       lon = lon_
--     }
