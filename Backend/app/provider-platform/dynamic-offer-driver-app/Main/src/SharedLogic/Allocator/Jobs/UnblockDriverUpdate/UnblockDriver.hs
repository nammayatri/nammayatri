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
    unblockSoftBlockedDriver,
  )
where

import qualified Domain.Types.DriverBlockTransactions as DTDBT
import Kernel.Beam.Functions as BF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
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
    EsqDBFlow m r
  ) =>
  Job 'UnblockDriver ->
  m ExecutionResult
unblockDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let driverId = jobData.driverId
  driver <- BF.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  let merchantId = driver.merchantId
  QDriverInfo.updateBlockedState (cast driverId) False (Just "AUTOMATICALLY_UNBLOCKED") merchantId driver.merchantOperatingCityId DTDBT.Application
  mbMerchantPN <- CPN.findMatchingMerchantPN driver.merchantOperatingCityId "UNBLOCK_DRIVER_KEY" Nothing Nothing driver.language Nothing
  whenJust mbMerchantPN $ \merchantPN -> do
    let entityData = NotifReq {entityId = driver.id.getId, title = merchantPN.title, message = merchantPN.body}
    notifyDriverOnEvents driver.merchantOperatingCityId driver.id driver.deviceToken entityData merchantPN.fcmNotificationType
  return Complete

unblockSoftBlockedDriver ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'UnblockSoftBlockedDriver ->
  m ExecutionResult
unblockSoftBlockedDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
  let driverId = jobData.driverId
  QDriverInfo.updateSoftBlock Nothing Nothing Nothing (cast driverId)
  return Complete
