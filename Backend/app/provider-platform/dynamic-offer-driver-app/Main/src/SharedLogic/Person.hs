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
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Scheduler.Environment
import Lib.Scheduler.JobStorageType.SchedulerType as JC
import SharedLogic.Allocator
import qualified SharedLogic.External.LocationTrackingService.Flow as LTS
import SharedLogic.External.LocationTrackingService.Types
import qualified Storage.Queries.DriverInformation as QDriverInformation
import qualified Storage.Queries.Person as QP
import Tools.Error
import Tools.Metrics

findPerson :: Id DP.Person -> Flow DP.Person
findPerson personId = do
  QP.findById personId
    >>= fromMaybeM (PersonNotFound personId.getId)

blockDriverTemporarily :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r, CoreMetrics m, HasLocationService m r, JobCreator r m, HasShortDurationRetryCfg r c) => Id DM.Merchant -> Id DMOC.MerchantOperatingCity -> Id DP.Person -> Text -> Int -> BlockReasonFlag -> m ()
blockDriverTemporarily merchantId merchantOperatingCityId driverId blockedReason blockTimeInHours blockReasonFlag = do
  now <- getCurrentTime
  logInfo $ "Temporarily blocking driver, driverId: " <> driverId.getId
  QDriverInformation.updateDynamicBlockedStateWithActivity driverId (Just blockedReason) (Just blockTimeInHours) "AUTOMATICALLY_BLOCKED_BY_APP" merchantId "AUTOMATICALLY_BLOCKED_BY_APP" merchantOperatingCityId DTDBT.Application True (Just False) (Just DriverInfo.OFFLINE) blockReasonFlag
  let expiryTime = addUTCTime (fromIntegral blockTimeInHours * 60 * 60) now
  void $ LTS.blockDriverLocationsTill merchantId driverId expiryTime
  let unblockDriverJobTs = secondsToNominalDiffTime (fromIntegral blockTimeInHours) * 60 * 60
  JC.createJobIn @_ @'UnblockDriver (Just merchantId) (Just merchantOperatingCityId) unblockDriverJobTs $
    UnblockDriverRequestJobData
      { driverId = driverId
      }
