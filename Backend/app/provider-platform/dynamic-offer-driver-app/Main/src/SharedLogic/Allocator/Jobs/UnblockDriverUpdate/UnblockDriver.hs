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
import Kernel.Beam.Functions as BF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified SharedLogic.DriverOnboarding.Status as SStatus
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.CachedQueries.Merchant.MerchantPushNotification as CPN
import qualified Storage.Queries.DriverInformation as QDriverInfo
import qualified Storage.Queries.Person as QPerson
import Tools.Error
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
  let merchantId = driver.merchantId
  SStatus.runBlockChange (cast driverId) $
    SStatus.Unblock
      SStatus.SimplePayload
        { SStatus.spModifier = Just "AUTOMATICALLY_UNBLOCKED",
          SStatus.spMerchantId = merchantId,
          SStatus.spMerchantOperatingCityId = driver.merchantOperatingCityId,
          SStatus.spBlockedBy = DTDBT.Application
        }
  mbMerchantPN <- CPN.findMatchingMerchantPN driver.merchantOperatingCityId "UNBLOCK_DRIVER_KEY" Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let entityData = NotifReq {entityId = driver.id.getId, title = merchantPN.title, message = merchantPN.body}
    notifyDriverOnEvents driver.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType
  return Complete

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
  QDriverInfo.updateSoftBlock Nothing Nothing Nothing (cast driverId)
  return Complete
