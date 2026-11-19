{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Turn-by-turn navigation routes for the driver app's in-app navigation.
--
-- The existing @/route@, @/pickup/route@ and @/trip/route@ APIs convert OSRM's answer into
-- 'Maps.GetRoutesResp', which keeps only the route points and totals: the turn type, direction,
-- road names and per-step distances that navigation needs are dropped. This API calls this city's
-- OSRM server directly and returns OSRM's route response unchanged, so the app's navigation engine
-- gets every step.
module Domain.Action.UI.NavigationRoute
  ( NavigationRouteReq (..),
    getNavigationRoute,
  )
where

import Control.Applicative ((<|>))
import qualified Data.Text as T
import qualified Domain.Types.Merchant as Merchant
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import qualified EulerHS.Types as Euler
import Kernel.External.Maps.OSRM.Config (OSRMCfg (..))
import Kernel.External.Maps.OSRM.RoadsClient (PointsList (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Types.SlidingWindowLimiter (APIRateLimitOptions (..))
import Kernel.Utils.Common
import qualified Kernel.Utils.ExternalAPICallLogging as ApiCallLogger
import Kernel.Utils.SlidingWindowLimiter (checkSlidingWindowLimitWithOptions)
import Servant hiding (throwError)
import qualified Tools.Maps as Maps

-- | Route from the driver's position through the remaining stops; the last waypoint is the destination.
data NavigationRouteReq = NavigationRouteReq
  { origin :: Maps.LatLong,
    -- | The driver's heading in degrees, when known. OSRM then starts the route in that direction,
    -- which avoids a route that begins with a U-turn behind the driver after a reroute.
    bearing :: Maybe Double,
    waypoints :: [Maps.LatLong],
    avoidTolls :: Maybe Bool,
    -- | For logging only: ties the call to a ride.
    rideId :: Maybe Text
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

-- | OSRM's route endpoint with every option navigation needs; the response is passed through as raw JSON.
type OsrmNavigationRouteAPI =
  "route"
    :> "v1"
    :> "driving"
    :> Capture "coordinates" PointsList
    :> MandatoryQueryParam "steps" Text
    :> MandatoryQueryParam "overview" Text
    :> MandatoryQueryParam "geometries" Text
    :> MandatoryQueryParam "annotations" Text
    :> MandatoryQueryParam "alternatives" Text
    :> MandatoryQueryParam "continue_straight" Text
    :> QueryParam "bearings" Text
    :> QueryParam "exclude" Text
    :> Get '[JSON] Value

-- | Returns this city's OSRM route for the request, unchanged.
--
-- Rate limited per driver as a safety net: the app already asks at most once per reroute, and
-- reroutes are at least 8 s apart. When tolls are to be avoided but this OSRM profile has no toll
-- class (the request fails), the route is fetched again without the exclusion.
getNavigationRoute ::
  ServiceFlow m r =>
  (Id DP.Person, Id Merchant.Merchant, Id DMOC.MerchantOperatingCity) ->
  NavigationRouteReq ->
  m Value
getNavigationRoute (personId, _merchantId, merchantOpCityId) req = do
  when (null req.waypoints) $ throwError $ InvalidRequest "waypoints must not be empty"
  checkSlidingWindowLimitWithOptions (navigationRouteHitsKey personId) navigationRouteRateLimit
  osrmCfg <- Maps.getOsrmServiceConfig merchantOpCityId
  let points = PointsList (req.origin : req.waypoints)
      bearings = bearingsParam (length req.waypoints) <$> req.bearing
      avoidTolls = fromMaybe False req.avoidTolls
      callOsrm exclude =
        callAPI
          osrmCfg.osrmUrl
          (Euler.client (Proxy @OsrmNavigationRouteAPI) points "true" "full" "polyline6" "distance,duration,speed" "false" "true" bearings exclude)
          "osrm-navigation-route"
          (Proxy @OsrmNavigationRouteAPI)
  firstTry <- callOsrm (if avoidTolls then Just "toll" else Nothing)
  rsp <- case firstTry of
    Left _ | avoidTolls -> callOsrm Nothing
    _ -> pure firstTry
  fork "Logging external API Call of OsrmNavigationRouteAPI OSRM" $
    ApiCallLogger.pushExternalApiCallDataToKafka "OsrmNavigationRouteAPI" "OSRM" (req.rideId <|> Just personId.getId) (Just merchantOpCityId.getId) (Just req) rsp
  fromEitherM (FailedToCallOsrmRouteAPI . show) rsp

-- | OSRM's @bearings@ value: the driver's heading (with a tolerance) for the origin, none for the waypoints.
bearingsParam :: Int -> Double -> Text
bearingsParam waypointCount heading =
  T.intercalate ";" $ (show (round heading `mod` 360 :: Int) <> "," <> show bearingToleranceDegrees) : replicate waypointCount ""

-- | How far (in degrees) OSRM may deviate from the driver's heading when choosing the start road.
bearingToleranceDegrees :: Int
bearingToleranceDegrees = 45

-- | At most 20 route requests per driver per minute: starts plus reroutes stay well below this.
navigationRouteRateLimit :: APIRateLimitOptions
navigationRouteRateLimit = APIRateLimitOptions {limit = 20, limitResetTimeInSec = 60}

navigationRouteHitsKey :: Id DP.Person -> Text
navigationRouteHitsKey personId = "NavigationRouteHits:" <> personId.getId <> ":hitsCount"
