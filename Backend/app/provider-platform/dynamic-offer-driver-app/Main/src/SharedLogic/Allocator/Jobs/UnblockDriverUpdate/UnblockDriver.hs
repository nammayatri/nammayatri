{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.UnblockDriverUpdate.UnblockDriver
  ( unblockDriver,
    unblockAirportDriver,
    unblockSoftBlockedDriver,
  )
where

import qualified Domain.Types.DriverBlockTransactions as DTDBT
import qualified Domain.Types.DriverInformation as DI
import qualified Domain.Types.MerchantPushNotification as Fields
import Domain.Types.TranslationKeys (DriverPushKey (..))
import Kernel.Beam.Functions as BF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.Allocator as Fields
import qualified SharedLogic.DriverOnboarding as SDO
import qualified SharedLogic.DriverOnboarding.OnboardingFlags.Flow as SFlags
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Fields as Fields
import Tools.Notifications

unblockDriver ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Hedis.HedisLTSFlowEnv r
  ) =>
  Job 'UnblockDriver ->
  m ExecutionResult
unblockDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let driverId = jobData.driverId
  driver <- BF.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  -- Fleet owners have no end time to check: unblocked as before.
  shouldUnblock <-
    if SDO.isFleetRole driver.role
      then pure True
      else do
        -- KV-first read (not the replica): the job runs right at the end time.
        mbDriverInfo <- QDriverInfo.findById (cast driverId)
        now <- getCurrentTime
        let isDue = maybe False (\driverInfo -> driverInfo.blocked && endTimeReached now driverInfo.blockExpiryTime) mbDriverInfo
        unless isDue $
          logInfo $ "UnblockDriver: driver " <> driverId.getId <> " left as is: not blocked, or the current block ends later or has no end time (blocked=" <> show ((.blocked) <$> mbDriverInfo) <> ", end=" <> show (mbDriverInfo >>= (.blockExpiryTime)) <> ")"
        pure isDue
  when shouldUnblock $ do
    void $
      SFlags.markBlockFlags (cast driverId) $
        SFlags.Unblock
          SFlags.SimplePayload
            { SFlags.spModifier = Just "AUTOMATICALLY_UNBLOCKED",
              SFlags.spMerchantId = driver.merchantId,
              SFlags.spMerchantOperatingCityId = driver.merchantOperatingCityId,
              SFlags.spBlockedBy = DTDBT.Application
            }
    mbMerchantPN <- CPN.findMatchingMerchantPNByKey driver.merchantOperatingCityId DriverPush_Driver_Unblocked Nothing Nothing driver.language Nothing
    whenJust mbMerchantPN $ \merchantPN -> do
      let entityData = mkNotifReq driver.id.getId (pushContentOfRow merchantPN [])
      notifyDriverOnEvents driver.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType
  return Complete

-- | Clock differences between the pod that stamps a block's end time and the pods that run
-- its unblock job: a job may lift a block up to this much early.
unblockClockAllowance :: NominalDiffTime
unblockClockAllowance = 60

-- | Has the end time passed (within the allowance)? No end time = never auto-lifted.
endTimeReached :: UTCTime -> Maybe UTCTime -> Bool
endTimeReached now = maybe False (<= addUTCTime unblockClockAllowance now)

unblockAirportDriver ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Hedis.HedisLTSFlowEnv r
  ) =>
  Job 'UnblockAirportDriver ->
  m ExecutionResult
unblockAirportDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let driverId = jobData.driverId
  driverInfo <- QDriverInfo.findById (cast driverId) >>= fromMaybeM DriverInfoNotFound
  now <- getCurrentTime
  when (driverInfo.enableForAirport == DI.BLOCKED && QDriverInfo.isAirportEligible now driverInfo) $ do
    let mbLogInfo = do
          mId <- driverInfo.merchantId
          moCityId <- driverInfo.merchantOperatingCityId
          pure
            QDriverInfo.AirportBlockLogInfo
              { blockedBy = DTDBT.Application,
                reason = Just "SCHEDULED_UNBLOCK",
                specialZoneId = jobData.specialZoneId,
                requestorId = Nothing,
                merchantId = mId,
                merchantOperatingCityId = moCityId
              }
    QDriverInfo.updateAirportSwitch DI.ENABLED Nothing mbLogInfo (cast driverId)
  return Complete

unblockSoftBlockedDriver ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r,
    Hedis.HedisLTSFlowEnv r
  ) =>
  Job 'UnblockSoftBlockedDriver ->
  m ExecutionResult
unblockSoftBlockedDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let driverId = jobData.driverId
  -- KV-first read. Leave the row as is only when a soft block is active and isn't due (a later one
  -- replaced the block this job was for, or it has no end time). Otherwise clear, even when the row
  -- already shows no soft block: the clear is repeatable (no push, no history row) and re-syncs the
  -- driver-pool copy, which a retry after a failed pool-copy sync would otherwise leave stale.
  mbDriverInfo <- QDriverInfo.findById (cast driverId)
  now <- getCurrentTime
  case mbDriverInfo of
    Nothing -> logInfo $ "UnblockSoftBlockedDriver: driver " <> driverId.getId <> " has no driver information, nothing to clear"
    Just driverInfo
      | QDriverInfo.hasSoftBlockTiers driverInfo.softBlockStiers && not (endTimeReached now driverInfo.softBlockExpiryTime) ->
        logInfo $ "UnblockSoftBlockedDriver: driver " <> driverId.getId <> " left as is: the current soft block ends later or has no end time (end=" <> show driverInfo.softBlockExpiryTime <> ")"
      | otherwise -> QDriverInfo.clearSoftBlock (cast driverId)
  return Complete
