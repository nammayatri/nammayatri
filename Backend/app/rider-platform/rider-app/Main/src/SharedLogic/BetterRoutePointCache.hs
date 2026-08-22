{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Keeps what a /rideSearch/ worked out about a route available after the response has
-- gone out, so a suggestion the customer taps minutes later can be priced without
-- redoing any of it.
--
-- Two things are parked here. The alternate shapes and the shadow searches created for
-- them, so /alternateSuggestion/{searchId}/result can collect their fares by search id
-- without the customer's app having to hand the geometry back. And the parent's route and
-- Beckn taggings, because pricing a point the customer invents later means trimming that
-- same polyline and re-sending those same tags; without them the only way to answer is a
-- fresh route lookup, which is exactly the map call this feature was built to avoid.
--
-- Written only when a suggestion was actually found, which is the uncommon case, so this
-- adds no Redis traffic to an ordinary search.
module SharedLogic.BetterRoutePointCache
  ( SuggestedSearchCtx (..),
    AlternateShadow (..),
    cacheSuggestedSearchCtx,
    getSuggestedSearchCtx,
    restoreSearchRes,
  )
where

import qualified BecknV2.OnDemand.Tags as Beckn
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.RiderPreferredOption as DRPO
import qualified Domain.Types.SearchRequest as DSearchReq
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Beckn.Context (City)
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.BetterRoutePoint as BRP
import qualified SharedLogic.Search as SLS
import qualified Tools.Maps as Maps

-- | A parent search's 'SLS.SearchRes' minus everything that can be read back cheaply:
-- the search request and merchant are one lookup each, and @now@ is the current time.
data SuggestedSearchCtx = SuggestedSearchCtx
  { origin :: SLS.SearchReqLocation,
    stops :: [SLS.SearchReqLocation],
    startTime :: UTCTime,
    returnTime :: Maybe UTCTime,
    riderPreferredOption :: DRPO.RiderPreferredOption,
    roundTrip :: Bool,
    gatewayUrl :: BaseUrl,
    searchRequestExpiry :: UTCTime,
    city :: City,
    device :: Maybe Text,
    distance :: Maybe Meters,
    duration :: Maybe Seconds,
    shortestRouteInfo :: Maybe Maps.RouteInfo,
    isReallocationEnabled :: Maybe Bool,
    fareParametersInRateCard :: Maybe Bool,
    -- | The parent's route geometry is already carried once, in 'shortestRouteInfo'.
    -- Everything a shadow needs is derived by trimming that, so the parent's alternative
    -- routes and the two tags restating them are dropped before this is written -- they
    -- are the bulk of the payload and every one of them is overridden downstream.
    taggings :: Maybe Beckn.Taggings,
    riderGender :: Maybe Text,
    -- | The walk-and-save shapes other than the default, ordered best first, each with the
    -- shadow search that is being priced for it.
    alternates :: [AlternateShadow]
  }
  deriving (Generic, ToJSON, FromJSON)

-- | One alternate shape and the shadow search created for it during /rideSearch.
--
-- The shadow exists from the moment the shape is found -- pricing it is dispatched in the
-- background rather than waited on -- so the customer's app can ask for its fare by search
-- id instead of describing the geometry back to us.
data AlternateShadow = AlternateShadow
  { searchId :: Id DSearchReq.SearchRequest,
    route :: BRP.BetterRoute
  }
  deriving (Generic, ToJSON, FromJSON)

suggestedSearchCtxKey :: Id DSearchReq.SearchRequest -> Text
suggestedSearchCtxKey parentSearchId = "betterRoutePoint:ctx:" <> parentSearchId.getId

-- | Held only as long as the search itself is answerable; a suggestion outliving the
-- estimates it sits next to would just fail later at select.
cacheSuggestedSearchCtx ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Id DSearchReq.SearchRequest ->
  SLS.SearchRes ->
  [AlternateShadow] ->
  m ()
cacheSuggestedSearchCtx parentSearchId SLS.SearchRes {..} alternates = do
  -- The search's own clock reading, not a fresh one: this is written while the search is
  -- still being answered, and the two are the same instant to any precision that matters.
  let ttl = round $ diffUTCTime searchRequestExpiry now
  when (ttl > 0) $
    Redis.setExp (suggestedSearchCtxKey parentSearchId) SuggestedSearchCtx {taggings = dropRouteTags <$> taggings, ..} ttl
  where
    -- Safe to drop rather than merely redundant: 'overrideRouteTaggings' appends a route
    -- tag the parent did not emit, so a shadow ends up with exactly the same tags either
    -- way -- describing the trimmed route, which is the only thing it is ever asked about.
    dropRouteTags parentTaggings =
      parentTaggings {Beckn.fulfillmentTags = filter ((`notElem` [Beckn.WAYPOINTS, Beckn.MULTIPLE_ROUTES]) . fst) parentTaggings.fulfillmentTags}

getSuggestedSearchCtx ::
  (MonadFlow m, Redis.HedisFlow m r) =>
  Id DSearchReq.SearchRequest ->
  m (Maybe SuggestedSearchCtx)
getSuggestedSearchCtx = Redis.safeGet . suggestedSearchCtxKey

-- | Rebuilds the parent's 'SLS.SearchRes' so a suggestion can be dispatched to the BPP by
-- the same code path the original search used.
restoreSearchRes ::
  MonadFlow m =>
  SuggestedSearchCtx ->
  DSearchReq.SearchRequest ->
  DM.Merchant ->
  m SLS.SearchRes
restoreSearchRes SuggestedSearchCtx {..} searchRequest merchant = do
  now <- getCurrentTime
  pure
    SLS.SearchRes
      { searchRequest,
        merchant,
        now,
        merchantOperatingCityId = searchRequest.merchantOperatingCityId,
        -- Restored only far enough to build a shadow of this search, never to re-dispatch
        -- the search itself, and a shadow is offered on one route: its own.
        multipleRoutes = Nothing,
        ..
      }
