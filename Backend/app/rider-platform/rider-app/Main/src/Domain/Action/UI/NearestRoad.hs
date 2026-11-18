module Domain.Action.UI.NearestRoad
  ( postNearestRoad,
  )
where

import qualified API.Types.UI.NearestRoad as API
import qualified Domain.Types.Merchant
import qualified Domain.Types.Person as Person
import qualified Environment
import EulerHS.Prelude hiding (id)
import qualified Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Tools.Error
import qualified Tools.Maps as Maps

-- | Snaps the given coordinate to the nearest road segment.
postNearestRoad ::
  ( Kernel.Prelude.Maybe (Id Person.Person),
    Id Domain.Types.Merchant.Merchant
  ) ->
  API.NearestRoadReq ->
  Environment.Flow API.NearestRoadResp
postNearestRoad (mbPersonId, merchantId) req = do
  personId <- mbPersonId & fromMaybeM (PersonNotFound "No person found")
  whenJust req.number $ \number ->
    when (number < 1) $ throwError (InvalidRequest "number must be at least 1")
  merchantOperatingCityId <- Maps.getMerchantOperatingCityId personId Nothing
  response <-
    Maps.getNearestPoints merchantId merchantOperatingCityId (Just personId.getId) $
      Maps.GetNearestPointsReq {point = req.location, number = req.number}
  pure $ API.NearestRoadResp {waypoints = map mkWaypoint response.waypoints}
  where
    mkWaypoint waypoint =
      API.NearestRoadWaypoint
        { location = waypoint.location,
          name = waypoint.roadName,
          distance = waypoint.distance
        }
