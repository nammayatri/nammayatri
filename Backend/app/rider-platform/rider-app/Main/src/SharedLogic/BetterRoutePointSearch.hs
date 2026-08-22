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
  ( SuggestedSearchBuild (..),
    buildSuggestedSearchRes,
    buildShadowSearchRes,
    resolveBetterRoute,
    betterPointConfig,
  )
where

import qualified BecknV2.OnDemand.Tags as Beckn
import Data.Aeson (encode)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text.Lazy as LT
import qualified Data.Text.Lazy.Encoding as TE
import qualified Domain.Types.Location as DL
import Domain.Types.LocationAddress (LocationAddress)
import qualified Domain.Types.RiderConfig as DRC
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.External.Maps.Types (LatLong (..))
import Kernel.External.Types (ServiceFlow)
import Kernel.Prelude
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.CalculateDistance (distanceBetweenInMeters)
import Kernel.Utils.Common
import qualified Lib.JourneyModule.Utils as JMU
import qualified SharedLogic.BetterRoutePoint as BRP
import qualified SharedLogic.BetterRoutePointCache as BRPC
import qualified SharedLogic.Search as SLS
import qualified Storage.Queries.SearchRequest as QSearchRequest
import Tools.Error
import qualified Tools.Maps as Maps

-- | What /rideSearch/ found: the shape it would pick, and the other shapes on offer.
--
-- Every one of them gets a shadow search request here, including the alternates. What
-- separates them is only how their fare is awaited: the default is joined before the
-- search response goes out, the alternates are dispatched fire-and-forget and collected
-- later through /alternateSuggestion/{searchId}/result.
data SuggestedSearchBuild = SuggestedSearchBuild
  { shadowSearchRes :: SLS.SearchRes,
    -- | Ready to dispatch, in the same order as 'alternates'.
    alternateSearchRes :: [SLS.SearchRes],
    alternates :: [BRPC.AlternateShadow]
  }

-- | Looks for a better pickup/drop on the route the parent search already resolved.
-- On a hit, persists the shadow search request for the best shape and returns a
-- 'SLS.SearchRes' for it that the caller can dispatch to the BPP exactly like the real
-- one, alongside the alternatives it deliberately left unpriced.
--
-- Returns 'Nothing' whenever the feature is off, the search is not a shape we can
-- reason about, or no point clears the configured thresholds — all of which are the
-- normal case, so callers should treat 'Nothing' as unremarkable.
buildSuggestedSearchRes ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  DRC.RiderConfig ->
  SLS.SearchRes ->
  m (Maybe SuggestedSearchBuild)
buildSuggestedSearchRes riderConfig parentRes = do
  -- `$!` matters here: without it the timing would be meaningless, because a lazy Maybe
  -- is not evaluated until used. Forcing to WHNF is enough — deciding Just vs Nothing is
  -- exactly what runs both segment scans.
  mbPlan <- JMU.measureLatency (pure $! detectBetterRoute riderConfig parentRes) "betterRoutePoint.detect"
  case mbPlan of
    Nothing -> pure Nothing
    Just plan -> JMU.measureLatency (Just <$> buildFromPlan plan) "betterRoutePoint.buildShadow"
  where
    buildFromPlan plan = do
      shadowSearchRes <- buildShadowSearchRes parentRes plan.best Nothing Nothing
      -- Alternates get their shadow now, not when the customer asks: creating it is two
      -- local writes, and doing it here is what lets their fare be dispatched in the
      -- background and collected by search id later. No address is resolved for any of
      -- them -- naming a point the customer may never choose would put a reverse-geocode
      -- on the search path, and select resolves the name of the one they do choose.
      alternateSearchRes <- traverse (\route -> buildShadowSearchRes parentRes route Nothing Nothing) plan.alternatives
      let alternates = zipWith (\res route -> BRPC.AlternateShadow {searchId = res.searchRequest.id, route}) alternateSearchRes plan.alternatives
      BRPC.cacheSuggestedSearchCtx parentRes.searchRequest.id parentRes alternates
      pure SuggestedSearchBuild {shadowSearchRes, alternateSearchRes, alternates}

-- | Persists a shadow search request for one better-route shape and returns the
-- 'SLS.SearchRes' that prices it. The address overrides are for endpoints the customer
-- chose themselves: when absent the parent's resolved address is carried over, which is
-- right for a point this module proposed a short walk away and wrong for one the customer
-- dragged somewhere else.
buildShadowSearchRes ::
  (MonadFlow m, CacheFlow m r, EsqDBFlow m r) =>
  -- | The parent search
  SLS.SearchRes ->
  BRP.BetterRoute ->
  -- | Address for the moved pickup, when the caller resolved one
  Maybe LocationAddress ->
  -- | Address for the moved drop, when the caller resolved one
  Maybe LocationAddress ->
  m SLS.SearchRes
buildShadowSearchRes parentRes betterRoute mbPickupAddress mbDropAddress = do
  let parent = parentRes.searchRequest
  childId <- generateGUID
  -- The suggested point is a short walk from the customer's own, so the parent's
  -- resolved address still describes it unless the caller has a better one. Reusing it
  -- keeps this off the reverse-geocode path, which /rideSearch is latency-sensitive about.
  childFrom <- maybe (pure parent.fromLocation) (relocate parent.fromLocation mbPickupAddress . (.point)) betterRoute.betterPickup
  childTo <- case (parent.toLocation, betterRoute.betterDrop) of
    (Just toLoc, Just betterDrop) -> Just <$> relocate toLoc mbDropAddress betterDrop.point
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
      <> " kind "
      <> show betterRoute.kind
      <> "; saved "
      <> show betterRoute.totalRideDistanceSaved
      <> ", walkToPickup "
      <> show ((.walkDistance) <$> betterRoute.betterPickup)
      <> ", walkFromDrop "
      <> show ((.walkDistance) <$> betterRoute.betterDrop)
  let childRouteInfo = trimRouteInfo betterRoute parentRes.shortestRouteInfo
  pure $
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
  where
    relocate loc mbAddress latLong = do
      locId <- generateGUID
      pure (loc {DL.id = locId, DL.lat = latLong.lat, DL.lon = latLong.lon, DL.address = fromMaybe loc.address mbAddress} :: DL.Location)

-- | The better-route shape for endpoints the customer picked -- an alternative they
-- tapped, or a marker they nudged off the ones offered.
--
-- Reading it off the parent's polyline is the whole point: the trim is arithmetic on a
-- route that was already fetched. A marker dragged clear of that route cannot be read off
-- it, and only then is a fresh route resolved, at the cost of one map call on a path the
-- customer is explicitly waiting on.
resolveBetterRoute ::
  (MonadFlow m, ServiceFlow m r) =>
  DRC.RiderConfig ->
  SLS.SearchRes ->
  -- | Chosen pickup, when that end moves
  Maybe LatLong ->
  -- | Chosen drop, when that end moves
  Maybe LatLong ->
  m BRP.BetterRoute
resolveBetterRoute riderConfig parentRes mbPickup mbDrop = do
  let parent = parentRes.searchRequest
      parentPickup = LatLong parent.fromLocation.lat parent.fromLocation.lon
  parentDropLoc <- parent.toLocation & fromMaybeM (InvalidRequest "Cannot suggest a better route point for a search without a destination")
  let parentDrop = LatLong parentDropLoc.lat parentDropLoc.lon
  validateWalk parentPickup mbPickup
  validateWalk parentDrop mbDrop
  let mbTrimmed = do
        routeInfo <- parentRes.shortestRouteInfo
        BRP.betterRouteForCustomPoints
          (fromMaybe defaultMaxOffRouteDistance riderConfig.betterPointMaxOffRouteDistance)
          parentPickup
          parentDrop
          routeInfo.points
          routeInfo.distance
          routeInfo.duration
          mbPickup
          mbDrop
  case mbTrimmed of
    Just betterRoute -> pure betterRoute
    Nothing -> do
      logInfo $ "better_route_point: chosen point is off the route of " <> getId parent.id <> ", resolving a fresh one"
      freshBetterRoute parentRes parentPickup parentDrop mbPickup mbDrop
  where
    -- A point the customer can walk to is the entire premise; anything further is a
    -- different ride, and belongs in a search of its own rather than a shadow of this one.
    --
    -- The absolute cap, not the ride-scaled one 'detectBetterRoute' offers points within.
    -- That headroom is deliberate: a customer nudging a marker that was already placed at
    -- the scaled cap has to be able to move it, and the point they land on is one they
    -- chose to walk to rather than one we talked them into.
    validateWalk own = \case
      Nothing -> pure ()
      Just chosen -> do
        let walk = highPrecMetersToMeters $ distanceBetweenInMeters own chosen
            maxWalk = fromMaybe defaultMaxWalkDistance riderConfig.maxWalkDistanceForBetterPoint
        when (walk > maxWalk) $
          throwError (InvalidRequest $ "Suggested point is " <> show walk <> " away, further than the " <> show maxWalk <> " a customer is asked to walk")

-- | The fallback for a point the parent's polyline cannot describe: ask the provider for
-- the route between the endpoints as they now stand, and state the saving against what
-- the parent search was quoted.
freshBetterRoute ::
  (MonadFlow m, ServiceFlow m r) =>
  SLS.SearchRes ->
  LatLong ->
  LatLong ->
  Maybe LatLong ->
  Maybe LatLong ->
  m BRP.BetterRoute
freshBetterRoute parentRes parentPickup parentDrop mbPickup mbDrop = do
  kind <- case (mbPickup, mbDrop) of
    (Just _, Just _) -> pure BRP.BOTH
    (Just _, Nothing) -> pure BRP.PICKUP
    (Nothing, Just _) -> pure BRP.DROP
    (Nothing, Nothing) -> throwError (InvalidRequest "A suggested fare needs at least one moved endpoint")
  let parent = parentRes.searchRequest
      newPickup = fromMaybe parentPickup mbPickup
      newDrop = fromMaybe parentDrop mbDrop
  routes <-
    Maps.getRoutes
      Nothing
      parent.riderId
      parent.merchantId
      (Just parent.merchantOperatingCityId)
      (Just parent.id.getId)
      Maps.GetRoutesReq {waypoints = NE.fromList [newPickup, newDrop], calcPoints = True, mode = Just Maps.CAR}
  routeInfo <- listToMaybe routes & fromMaybeM (InvalidRequest "No route found between the suggested points")
  newRouteDistance <- routeInfo.distance & fromMaybeM (InvalidRequest "Route to the suggested points has no distance")
  pure
    BRP.BetterRoute
      { kind,
        betterPickup = mbPickup <&> \p -> BRP.BetterPoint {point = p, walkDistance = walkFrom parentPickup p, rideDistanceSaved = Nothing},
        betterDrop = mbDrop <&> \p -> BRP.BetterPoint {point = p, walkDistance = walkFrom parentDrop p, rideDistanceSaved = Nothing},
        -- Negative when the customer moved a marker somewhere that lengthens the ride.
        -- That is their call to make; reporting it honestly is what lets them unmake it.
        totalRideDistanceSaved = maybe 0 (subtract newRouteDistance) parentRes.distance,
        newRouteDistance,
        newRouteDuration = routeInfo.duration,
        newRoutePoints = routeInfo.points
      }
  where
    walkFrom own chosen = highPrecMetersToMeters $ distanceBetweenInMeters own chosen

defaultMaxOffRouteDistance :: Meters
defaultMaxOffRouteDistance = Meters 60

defaultMaxWalkDistance :: Meters
defaultMaxWalkDistance = Meters 400

-- | All the reasons a search is not a candidate, in one place.
detectBetterRoute :: DRC.RiderConfig -> SLS.SearchRes -> Maybe BRP.BetterRoutePlan
detectBetterRoute riderConfig parentRes = do
  guard (fromMaybe False riderConfig.enableBetterRoutePointSuggestion)
  cfg <- betterPointConfig riderConfig
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
    cfg
    (LatLong parent.fromLocation.lat parent.fromLocation.lon)
    (LatLong toLocation.lat toLocation.lon)
    routeInfo.points
    routeInfo.distance
    routeInfo.duration

-- | 'Nothing' when the operating city has not been given the thresholds, which is how the
-- feature stays off by default.
betterPointConfig :: DRC.RiderConfig -> Maybe BRP.BetterPointConfig
betterPointConfig riderConfig = do
  minSaving <- riderConfig.minRideDistanceSavingForBetterPoint
  maxWalk <- riderConfig.maxWalkDistanceForBetterPoint
  pure
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
