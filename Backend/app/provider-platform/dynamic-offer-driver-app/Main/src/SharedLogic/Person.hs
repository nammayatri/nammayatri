{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Person where

import qualified Domain.Types.Common as DriverInfo
import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.Merchant as DM
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as DP
import Environment
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Flow as SFlags
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import qualified SharedLogic.PendingOnRideBlock as PendingBlock
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics

findPerson :: Id DP.Person -> Flow DP.Person
findPerson personId = do
  QP.findById personId
    >>= fromMaybeM (PersonNotFound personId.getId)

blockDriverTemporarily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m, HasShortDurationRetryCfg r c, Hedis.HedisLTSFlowEnv r) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DP.Person -> Text -> Int -> BlockReasonFlag -> m ()
blockDriverTemporarily merchantId merchantOperatingCityId driverId blockedReason blockTimeInHours blockReasonFlag = do
  now <- getCurrentTime
  logInfo $ "Temporarily blocking driver, driverId: " <> driverId.getId
  SFlags.markBlockFlags driverId $
    SFlags.Block
      SFlags.BlockPayload
        { SFlags.bpReason = Just blockedReason,
          SFlags.bpExpiryHours = Just blockTimeInHours,
          SFlags.bpDashboardUserName = "AUTOMATICALLY_BLOCKED_BY_APP",
          SFlags.bpMerchantId = merchantId,
          SFlags.bpReasonCode = "AUTOMATICALLY_BLOCKED_BY_APP",
          SFlags.bpMerchantOperatingCityId = merchantOperatingCityId,
          SFlags.bpBlockedBy = DTDBT.Application,
          SFlags.bpActive = Just False,
          SFlags.bpMode = Just DriverInfo.OFFLINE,
          SFlags.bpFlag = blockReasonFlag
        }
  let expiryTime = addUTCTime (fromIntegral blockTimeInHours * 60 * 60) now
  void $ LTS.blockDriverLocationsTill merchantId driverId expiryTime
  let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral blockTimeInHours) * 60 * 60
  JC.createJobIn @_ @'UnblockDriver (Just merchantId) (Just merchantOperatingCityId) unblockDriverJobTs $
    UnblockDriverRequestJobData
      { driverId = driverId
      }

-- | Apply a dashboard block that was parked while the driver was on a ride (see
--   SharedLogic.PendingOnRideBlock). Call once the driver has been taken off the ride. A driver
--   who is still on a ride (an advance booking picked up before the block was requested) keeps
--   the pending block and is blocked when that ride ends instead.
--
--   Never throws: the caller is a ride-end or cancel path that must finish regardless, so a
--   failure is logged and the pending block is kept for the next attempt. A short Redis lock
--   keeps two concurrent callers (ride end and a stuck-on-ride repair, say) from blocking twice.
applyPendingOnRideBlock :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, JobCreator r m, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> m ()
applyPendingOnRideBlock driverId = do
  mbPending <- PendingBlock.getPendingOnRideBlock driverId
  whenJust mbPending $ \pending -> do
    let lockKey = "driver-offer:PendingOnRideBlock:apply:" <> driverId.getId
    lockAcquired <- Hedis.tryLockRedis lockKey 10
    when lockAcquired $ do
      result <- withTryCatch "applyPendingOnRideBlock" $ applyPendingOnRideBlock' driverId pending
      Hedis.unlockRedis lockKey
      case result of
        Right () -> pure ()
        Left err -> logError $ "Pending dashboard block not applied, kept for the next attempt, driverId: " <> driverId.getId <> ", error: " <> show err

applyPendingOnRideBlock' :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, JobCreator r m, Hedis.HedisLTSFlowEnv r) => Id DP.Person -> PendingBlock.PendingOnRideBlock -> m ()
applyPendingOnRideBlock' driverId pending = do
  mbDriverInfo <- QDI.findById (cast driverId)
  if maybe False (.onRide) mbDriverInfo
    then logInfo $ "Pending dashboard block kept, driver still on a ride, driverId: " <> driverId.getId
    else do
      if maybe False (.blocked) mbDriverInfo
        then logInfo $ "Pending dashboard block dropped, driver already blocked, driverId: " <> driverId.getId
        else do
          driver <- QP.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
          logInfo $ "Applying pending dashboard block after ride, driverId: " <> driverId.getId <> ", requestedAt: " <> show pending.requestedAt
          SFlags.markBlockFlags driverId $
            SFlags.Block
              SFlags.BlockPayload
                { SFlags.bpReason = pending.blockReason,
                  SFlags.bpExpiryHours = pending.blockTimeInHours,
                  SFlags.bpDashboardUserName = pending.dashboardUserName,
                  SFlags.bpMerchantId = driver.merchantId,
                  SFlags.bpReasonCode = pending.reasonCode,
                  SFlags.bpMerchantOperatingCityId = driver.merchantOperatingCityId,
                  SFlags.bpBlockedBy = DTDBT.Dashboard,
                  SFlags.bpActive = Nothing,
                  SFlags.bpMode = Nothing,
                  SFlags.bpFlag = ByDashboard
                }
          whenJust pending.blockTimeInHours $ \hrs -> do
            let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral hrs) * 60 * 60
            JC.createJobIn @_ @'UnblockDriver (Just driver.merchantId) (Just driver.merchantOperatingCityId) unblockDriverJobTs $
              UnblockDriverRequestJobData
                { driverId = driverId
                }
      -- Only drop the request we handled: a newer one parked meanwhile waits for the next ride end.
      mbCurrent <- PendingBlock.getPendingOnRideBlock driverId
      when (((.requestedAt) <$> mbCurrent) == Just pending.requestedAt) $ PendingBlock.clearPendingOnRideBlock driverId
