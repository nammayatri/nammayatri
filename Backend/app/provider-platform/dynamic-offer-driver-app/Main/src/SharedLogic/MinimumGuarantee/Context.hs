{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

-- | Per-driver Redis entry of the Minimum Guarantee (MG) days a driver is enrolled in. The ride,
-- cancel, mode-change and allocator hooks run for every driver, and almost none are enrolled, so
-- they first look here: a driver with no entry costs one Redis read and no database query.
--
-- The entry is written when a driver (or their fleet owner) opts in for a day, holds the day's
-- windows as absolute times, and expires once the day has settled.
module SharedLogic.MinimumGuarantee.Context
  ( MgEventFlow,
    MgDayCtx (..),
    registerMgCtx,
    unregisterMgCtx,
    getMgCtxs,
    getMgForceAssignDriverIds,
    setMgCampaignPaused,
    withMgDriverLock,
  )
where

import qualified Control.Monad.Catch as C
import Data.List (nub)
import qualified Data.Map.Strict as Map
import qualified Data.Set as Set
import Data.Time (Day)
import qualified Domain.Types.MgCampaign as DMgC
import qualified Domain.Types.MgEnrollment as DMgE
import qualified Domain.Types.Person as DP
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Types.Id
import Kernel.Utils.Common
import SharedLogic.MinimumGuarantee.Rules (MgWindowSpan, insideAnySpan)

-- | What the MG event, enrollment and settlement code needs from the monad.
type MgEventFlow m r =
  ( MonadFlow m,
    EsqDBFlow m r,
    CacheFlow m r,
    Redis.HedisFlow m r,
    Redis.HedisLTSFlowEnv r
  )

-- | One enrolled day of one driver.
data MgDayCtx = MgDayCtx
  { enrollmentId :: Id DMgE.MgEnrollment,
    campaignId :: Id DMgC.MgCampaign,
    periodDate :: Day,
    forceAssign :: Bool,
    spans :: [MgWindowSpan],
    -- | last window end + settlement grace; after this the day is settled and the entry is dead
    expiresAt :: UTCTime
  }
  deriving stock (Show, Eq, Generic)
  deriving anyclass (FromJSON, ToJSON)

mgCtxKey :: Id DP.Person -> Text
mgCtxKey driverId = "MgCtx:" <> driverId.getId

mgLockKey :: Id DP.Person -> Text
mgLockKey driverId = "MgLock:" <> driverId.getId

-- | Serialises every MG read-modify-write of one driver's enrollment rows and context. A ride end,
-- a cancel and a mode change of the same driver can arrive together, and the counters live in one
-- JSON column, so an unlocked update could overwrite another's.
--
-- Written with 'MonadCatch' (part of 'Redis.HedisFlow') instead of the kernel's 'MonadMask' lock
-- helpers, so the hooks do not add a constraint to every function between them and the callers.
-- The lock expires after 10 seconds, so a crashed holder cannot block the driver for long.
withMgDriverLock :: Redis.HedisFlow m r => Id DP.Person -> m a -> m a
withMgDriverLock driverId action = do
  acquire
  result <- C.try action
  Redis.unlockRedis (mgLockKey driverId)
  case result of
    Left (e :: SomeException) -> C.throwM e
    Right a -> pure a
  where
    acquire = do
      locked <- Redis.tryLockRedis (mgLockKey driverId) 10
      unless locked $ liftIO (threadDelay 100000) >> acquire

-- | The driver's live MG days. Entries whose settlement time has passed are dropped.
getMgCtxs :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> m [MgDayCtx]
getMgCtxs driverId = do
  now <- getCurrentTime
  stored :: Maybe [MgDayCtx] <- Redis.withCrossAppRedis $ Redis.safeGet (mgCtxKey driverId)
  pure $ filter (\c -> c.expiresAt > now) (fromMaybe [] stored)

-- | Adds (or replaces) a day in the driver's entry. Caller holds 'withMgDriverLock'.
registerMgCtx :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> MgDayCtx -> m ()
registerMgCtx driverId ctx = do
  existing <- getMgCtxs driverId
  writeCtxs driverId (ctx : filter (\c -> c.enrollmentId /= ctx.enrollmentId) existing)

-- | Removes a day, e.g. on opt-out or cancellation. Caller holds 'withMgDriverLock'.
unregisterMgCtx :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> Id DMgE.MgEnrollment -> m ()
unregisterMgCtx driverId enrollmentId = do
  existing <- getMgCtxs driverId
  writeCtxs driverId (filter (\c -> c.enrollmentId /= enrollmentId) existing)

writeCtxs :: (MonadFlow m, Redis.HedisFlow m r) => Id DP.Person -> [MgDayCtx] -> m ()
writeCtxs driverId ctxs = do
  now <- getCurrentTime
  case ctxs of
    [] -> Redis.withCrossAppRedis $ void $ Redis.del (mgCtxKey driverId)
    (firstCtx : otherCtxs) -> do
      let latest = foldl' (\latestSoFar c -> max latestSoFar c.expiresAt) firstCtx.expiresAt otherCtxs
          ttlSeconds = max 60 (ceiling (diffUTCTime latest now)) + 3600
      Redis.withCrossAppRedis $ Redis.setExp (mgCtxKey driverId) ctxs ttlSeconds

-- | Marks a campaign paused or running again. A paused campaign records no violations and does not
-- force-assign rides; the flag is read together with the drivers' entries, so no per-driver entry has
-- to be rewritten when a campaign is paused. It expires after a year, far beyond any pause.
setMgCampaignPaused :: (MonadFlow m, Redis.HedisFlow m r) => Id DMgC.MgCampaign -> Bool -> m ()
setMgCampaignPaused campaignId paused =
  Redis.withCrossAppRedis $
    if paused
      then Redis.setExp (mgPausedKey campaignId) True (365 * 24 * 3600)
      else void $ Redis.del (mgPausedKey campaignId)

mgPausedKey :: Id DMgC.MgCampaign -> Text
mgPausedKey campaignId = "MgPaused:" <> campaignId.getId

-- | Which of these drivers are inside a force-assign window right now. One batched Redis read for
-- the whole candidate batch; drivers with no entry cost nothing. A second read, only for the
-- campaigns that matched, drops the ones that are paused.
getMgForceAssignDriverIds :: (MonadFlow m, Redis.HedisFlow m r) => [Id DP.Person] -> UTCTime -> m (Set.Set (Id DP.Person))
getMgForceAssignDriverIds [] _ = pure Set.empty
getMgForceAssignDriverIds driverIds now = do
  pairs <- Redis.withCrossAppRedis $ Redis.mGetClusterWithKeys @[MgDayCtx] (map mgCtxKey driverIds)
  let byKey = Map.fromList pairs
      matching =
        [ (d, c.campaignId)
          | d <- driverIds,
            Just ctxs <- [Map.lookup (mgCtxKey d) byKey],
            c <- ctxs,
            c.forceAssign && insideAnySpan now c.spans
        ]
  if null matching
    then pure Set.empty
    else do
      paused <- Redis.withCrossAppRedis $ Redis.mGetClusterWithKeys @Bool (map mgPausedKey (nub [campaignId | (_driverId, campaignId) <- matching]))
      let pausedKeys = Set.fromList [key | (key, _paused) <- paused]
      pure $ Set.fromList [d | (d, campaignId) <- matching, mgPausedKey campaignId `Set.notMember` pausedKeys]
