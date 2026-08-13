{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Turns a detour found by 'SharedLogic.BetterRoutePoint' into a shadow search
-- request that can be priced by the BPP independently of the real one.
--
-- The shadow carries its own id, so it becomes its own Beckn transaction. That
-- matters more than it looks: the BPP caches the route for a transaction under
-- @searchRequestKey transactionId@ and reads it back for the whole ride
-- (navigation, location updates, end-ride distance checks). Two searches sharing
-- one transaction id would share that one slot, and a customer who picked the
-- suggestion would be driven along the route they were trying to avoid.
--
-- The customer's own search is never touched. If they pick a suggested estimate,
-- select/init/confirm resolve the shadow through @estimate.requestId@ with no
-- special handling anywhere downstream.
module SharedLogic.BetterRoutePointSearch
  ( buildSuggestedSearchRes,
  )
where

import qualified BecknV2.OnDemand.Tags as Beckn
import Data.Aeson (encode)
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Types.Location as DL
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.BetterRoutePoint as BRP
import qualified SharedLogic.Search as SLS
import qualified Storage.Queries.SearchRequest as QSearchRequest
import qualified Tools.Maps as Maps

-- | Looks for a better pickup/drop on the route the parent search already resolved.
-- On a hit, persists the shadow search request and returns a 'SLS.SearchRes' for it
-- that the caller can dispatch to the BPP exactly like the real one.
--
-- Returns 'Nothing' whenever the feature is off, the search is not a shape we can
-- reason about, or no point clears the configured thresholds — all of which are the
-- normal case, so callers should treat 'Nothing' as unremarkable.
buildSuggestedSearchRes ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DRC.RiderConfig ->
  SLS.SearchRes ->
  m (Maybe SLS.SearchRes)
buildSuggestedSearchRes riderConfig parentRes = do
  -- `$!` matters here: without it the timing would be meaningless, because a lazy Maybe
  -- is not evaluated until used. Forcing to WHNF is enough — deciding Just vs Nothing is
  -- exactly what runs both segment scans.
  mbBetterRoute <- JMU.measureLatency (pure $! detectBetterRoute riderConfig parentRes) "betterRoutePoint.detect"
  case mbBetterRoute of
    Nothing -> pure Nothing
    Just betterRoute -> JMU.measureLatency (buildShadow betterRoute) "betterRoutePoint.buildShadow"
  where
    buildShadow betterRoute = do
      let parent = parentRes.searchRequest
      childId <- generateGUID
      -- The suggested point is a short walk from the customer's own, so the parent's
      -- resolved address still describes it. Reusing it keeps this off the
      -- reverse-geocode path, which /rideSearch is latency-sensitive about.
      childFrom <- maybe (pure parent.fromLocation) (relocate parent.fromLocation . (.point)) betterRoute.betterPickup
      childTo <- case (parent.toLocation, betterRoute.betterDrop) of
        (Just toLoc, Just betterDrop) -> Just <$> relocate toLoc betterDrop.point
        (mbToLoc, _) -> pure mbToLoc
      let childDistance = convertMetersToDistance parent.distanceUnit betterRoute.newRouteDistance
          child =
            parent
              { DSearchReq.id = childId,
                DSearchReq.fromLocation = childFrom,
                DSearchReq.toLocation = childTo,
                DSearchReq.distance = Just childDistance,
                DSearchReq.estimatedRideDuration = betterRoute.newRouteDuration,
                -- The trimmed leg's static duration is not separately known; leaving it
                -- unset is better than carrying the parent's, which describes a longer route.
                DSearchReq.estimatedRideStaticDuration = Nothing,
                DSearchReq.parentSearchRequestId = Just parent.id,
                DSearchReq.betterPointWalkToPickup = (.walkDistance) <$> betterRoute.betterPickup,
                DSearchReq.betterPointWalkFromDrop = (.walkDistance) <$> betterRoute.betterDrop,
                DSearchReq.betterPointRideDistanceSaved = Just betterRoute.totalRideDistanceSaved
              }
      QSearchRequest.createDSReq child
      logInfo $
        "better_route_point: created shadow search " <> getId childId
          <> " for parent "
          <> getId parent.id
          <> "; saved "
          <> show betterRoute.totalRideDistanceSaved
          <> ", walkToPickup "
          <> show ((.walkDistance) <$> betterRoute.betterPickup)
          <> ", walkFromDrop "
          <> show ((.walkDistance) <$> betterRoute.betterDrop)
      let childRouteInfo = trimRouteInfo betterRoute parentRes.shortestRouteInfo
      pure . Just $
        parentRes
          { SLS.searchRequest = child,
            SLS.distance = Just betterRoute.newRouteDistance,
            SLS.duration = betterRoute.newRouteDuration,
            SLS.shortestRouteInfo = childRouteInfo,
            -- Only the trimmed route is on offer, so the BPP must not be given the
            -- parent's alternatives to pick a cheaper one from.
            SLS.multipleRoutes = (: []) <$> childRouteInfo,
            SLS.origin = SLS.SearchReqLocation {gps = LatLong childFrom.lat childFrom.lon, address = childFrom.address},
            SLS.taggings = overrideRouteTaggings betterRoute childRouteInfo parentRes.taggings
          }

    relocate loc latLong = do
      locId <- generateGUID
      pure (loc {DL.id = locId, DL.lat = latLong.lat, DL.lon = latLong.lon} :: DL.Location)

-- | All the reasons a search is not a candidate, in one place.
detectBetterRoute :: DRC.RiderConfig -> SLS.SearchRes -> Maybe BRP.BetterRoute
detectBetterRoute riderConfig parentRes = do
  guard (fromMaybe False riderConfig.enableBetterRoutePointSuggestion)
  minSaving <- riderConfig.minRideDistanceSavingForBetterPoint
  maxWalk <- riderConfig.maxWalkDistanceForBetterPoint
  let parent = parentRes.searchRequest
  -- A shadow of a shadow would recurse; a round trip ends where it starts, so
  -- "walk forward along the route" is meaningless for it.
  guard (isNothing parent.parentSearchRequestId)
  guard (parent.roundTrip /= Just True)
  -- Intermediate stops make it ambiguous which leg a trim applies to.
  guard (null parent.stops)
  guard (parent.isMeterRideSearch /= Just True)
  guard (parent.isMultimodalSearch /= Just True)
  -- A reserved (subscription) ride has a standing pickup the customer already committed to,
  -- and its on_search auto-selects the cheapest estimate -- so a shadow here would book
  -- itself. Domain.Action.Beckn.OnSearch also guards that; this keeps us off the path
  -- entirely.
  guard (parent.searchMode /= Just DSearchReq.RESERVE)
  toLocation <- parent.toLocation
  routeInfo <- parentRes.shortestRouteInfo
  BRP.findBetterRoutePoints
    BRP.BetterPointConfig
      { minRideDistanceSaving = minSaving,
        maxWalkDistance = maxWalk,
        -- 3% walk / 5% saving: on a 9.5km ride that allows a ~285m walk, which is what it
        -- takes to reach detours that a flat 150m cap silently hides; on a 2km ride it
        -- allows only ~60m.
        maxWalkPctOfRide = fromMaybe 0.03 riderConfig.betterPointMaxWalkPctOfRide,
        minSavingPctOfRide = fromMaybe 0.05 riderConfig.betterPointMinSavingPctOfRide,
        -- Default 5: enough that a short walk beats the walk cap for a modest extra
        -- saving, without being so high that only near-zero walks ever win.
        walkAversion = fromMaybe 5 riderConfig.betterPointWalkAversion
      }
    (LatLong parent.fromLocation.lat parent.fromLocation.lon)
    (LatLong toLocation.lat toLocation.lon)
    routeInfo.points
    routeInfo.distance
    routeInfo.duration

-- | The parent's route with the trimmed geometry and distances substituted in.
trimRouteInfo :: BRP.BetterRoute -> Maybe Maps.RouteInfo -> Maybe Maps.RouteInfo
trimRouteInfo betterRoute =
  fmap $ \routeInfo ->
    routeInfo
      { Maps.points = betterRoute.newRoutePoints,
        Maps.distance = Just betterRoute.newRouteDistance,
        Maps.distanceWithUnit = flip convertMetersToDistance betterRoute.newRouteDistance . (.unit) <$> routeInfo.distanceWithUnit,
        Maps.duration = betterRoute.newRouteDuration,
        Maps.staticDuration = Nothing,
        -- Snapped waypoints and the bounding box describe the untrimmed route; a stale
        -- box would be worse than none.
        Maps.snappedWaypoints = [],
        Maps.boundingBox = Nothing
      }

-- | The BPP reads distance, duration and geometry off fulfillment tags
-- (see @getRouteServiceability@ in the driver app), so the shadow's tags have to
-- describe the trimmed route rather than the parent's.
overrideRouteTaggings :: BRP.BetterRoute -> Maybe Maps.RouteInfo -> Maybe Beckn.Taggings -> Maybe Beckn.Taggings
overrideRouteTaggings betterRoute childRouteInfo =
  fmap $ \taggings ->
    taggings
      { Beckn.fulfillmentTags = foldl' replaceTag taggings.fulfillmentTags overrides
      }
  where
    overrides =
      [ (Beckn.DISTANCE_INFO_IN_M, Just . show . getMeters $ betterRoute.newRouteDistance),
        (Beckn.DURATION_INFO_IN_S, show . getSeconds <$> betterRoute.newRouteDuration),
        (Beckn.WAYPOINTS, Just . encodeTag $ betterRoute.newRoutePoints),
        (Beckn.MULTIPLE_ROUTES, encodeTag . (: []) <$> childRouteInfo)
      ]

    encodeTag :: ToJSON a => a -> Text
    encodeTag = LT.toStrict . TE.decodeUtf8 . encode

    -- Replace in place when the parent set the tag, otherwise append, so a tag the
    -- parent happened not to emit still reaches the BPP.
    replaceTag :: Beckn.TagList -> (Beckn.BecknTag, Maybe Text) -> Beckn.TagList
    replaceTag tags (tagKey, tagValue)
      | any ((== tagKey) . fst) tags = map (\entry -> if fst entry == tagKey then (tagKey, tagValue) else entry) tags
      | otherwise = tags <> [(tagKey, tagValue)]
