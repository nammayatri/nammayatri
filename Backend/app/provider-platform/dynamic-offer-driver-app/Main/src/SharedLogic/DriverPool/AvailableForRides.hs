{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Driver-initiated "available for rides" boost.
--
-- A driver switches it on from the app (Domain.Action.UI.AvailableForRides); that writes
-- the @AvailableForRides\#true\#\<expiredAt\>@ tag onto 'person.driverTag', which then rides
-- along into the driver pool the same way every other namma tag does.
--
-- The tag has two independent lifetimes, and whichever runs out first ends the boost:
--
--   * time    -- the expiry stamped into the tag itself. Expired tags are dropped on every
--               read of 'person.driverTag' ('Lib.Yudhishthira.Tools.Utils.filterExpiredTags')
--               and filtered out of the pool's tag JSON, so nothing has to sweep them.
--   * volume  -- @availableForRidesMaxSearchRequests@ search requests. Counted in Redis by
--               the allocator as it dispatches, and the tag is removed from the driver the
--               moment the budget is spent.
--
-- A per-driver, per-local-day Redis counter caps how many times the boost can be switched
-- on at all; re-activating while a boost is live is allowed but costs another activation
-- and restarts both lifetimes.
module SharedLogic.DriverPool.AvailableForRides
  ( availableForRidesTagName,
    availableForRidesTagNameValue,
    mkAvailableForRidesTag,
    hasAvailableForRidesTag,
    getActivationsUsedToday,
    recordActivation,
    startRequestBudget,
    recordRequestSent,
    dropTag,
  )
where

import qualified Data.Aeson as A
import qualified Data.Time as DT
import EulerHS.Prelude hiding (id)
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Common
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Lib.Yudhishthira.Tools.Utils as Yudhishthira
import qualified Lib.Yudhishthira.Types as LYT
import qualified Storage.Queries.Person as QPerson

availableForRidesTagName :: LYT.TagName
availableForRidesTagName = LYT.TagName "AvailableForRides"

availableForRidesTagValue :: LYT.TagValue
availableForRidesTagValue = LYT.TextValue "true"

-- | The name+value pair, for the tag-list helpers that match on it.
availableForRidesTagNameValue :: LYT.TagNameValue
availableForRidesTagNameValue = Yudhishthira.mkTagNameValue availableForRidesTagName availableForRidesTagValue

-- | The tag as written onto the driver, carrying its expiry timestamp.
mkAvailableForRidesTag :: Minutes -> UTCTime -> LYT.TagNameValueExpiry
mkAvailableForRidesTag validity =
  Yudhishthira.mkTagNameValueExpiryInMinutes availableForRidesTagName availableForRidesTagValue (Just validity)

-- | Read the flag off a pool result's tag JSON (the '.driverTags' object built by
-- 'Storage.Queries.Person.GetNearestDrivers.mkResultHelper', which is already
-- expiry-filtered) rather than re-fetching the driver mid-dispatch.
hasAvailableForRidesTag :: A.Value -> Bool
hasAvailableForRidesTag = isJust . Yudhishthira.accessTagKey availableForRidesTagName

-- | Keyed by local day so the daily allowance resets at the city's midnight, not UTC's.
activationCountKey :: Id person -> DT.Day -> Text
activationCountKey driverId day = "driver:availableForRides:activations:" <> driverId.getId <> ":" <> show day

requestCountKey :: Id person -> Text
requestCountKey driverId = "driver:availableForRides:requests:" <> driverId.getId

-- | Kept generous: the key is already day-scoped, this TTL only stops it lingering.
activationCountTtl :: Int
activationCountTtl = 172800 -- 2 days

getActivationsUsedToday :: (Redis.HedisFlow m r) => Id person -> DT.Day -> m Int
getActivationsUsedToday driverId localDay =
  Redis.withCrossAppRedis $ fromMaybe 0 <$> Redis.get @Int (activationCountKey driverId localDay)

-- | Counts one activation and reports the new total, so the caller can compare it
-- against the configured daily allowance without a second round trip.
recordActivation :: (Redis.HedisFlow m r) => Id person -> DT.Day -> m Int
recordActivation driverId localDay = Redis.withCrossAppRedis $ do
  let key = activationCountKey driverId localDay
  count <- Redis.incr key
  when (count == 1) $ Redis.expire key activationCountTtl
  pure $ fromIntegral count

-- | (Re)start the request budget for a fresh activation. The TTL matches the tag's own
-- validity, so a boost that runs out of time leaves nothing behind.
startRequestBudget :: (Redis.HedisFlow m r) => Id person -> Minutes -> m ()
startRequestBudget driverId validity =
  Redis.withCrossAppRedis $ Redis.setExp (requestCountKey driverId) (0 :: Int) (validity.getMinutes * 60)

-- | Charge one dispatched search request against the driver's budget, and take the tag
-- away once it is spent. Only ever called for drivers that actually carried the tag.
recordRequestSent ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id person ->
  Minutes ->
  Int ->
  m ()
recordRequestSent driverId validity maxRequests = do
  count <- Redis.withCrossAppRedis $ do
    let key = requestCountKey driverId
    count <- Redis.incr key
    -- The budget key is seeded at activation; if it has gone missing, re-bound it so a
    -- stray counter can't outlive the boost.
    when (count == 1) $ Redis.expire key (validity.getMinutes * 60)
    pure (fromIntegral count :: Int)
  when (count >= maxRequests) $ do
    logInfo $ "AvailableForRides budget exhausted for driver " <> driverId.getId <> ", dropping tag"
    dropTag driverId

-- | Remove the tag from the driver (and clear the budget counter). A no-op when the
-- driver no longer holds it, so it is safe to call from any of the expiry paths.
dropTag ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r, Redis.HedisFlow m r, Redis.HedisLTSFlowEnv r) =>
  Id person ->
  m ()
dropTag driverId = do
  let personId = cast driverId
  mbPerson <- QPerson.findById personId
  whenJust mbPerson $ \person ->
    -- findById already drops expired tags, so `retained` is also the cleaned-up list.
    let retained = Yudhishthira.removeTagName person.driverTag availableForRidesTagNameValue
     in when (length retained /= length (fromMaybe [] person.driverTag)) $ do
          QPerson.updateDriverTag (if null retained then Nothing else Just retained) personId
          Redis.withCrossAppRedis $ Redis.del (requestCountKey driverId)
