{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.FCM.SoftBlockNotification
  ( softBlockNotifyDriver,
  )
where

import Kernel.Beam.Functions as BF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import qualified Kernel.Storage.Hedis as Redis
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import SharedLogic.GoogleTranslate (TranslateFlow)
import qualified Storage.Queries.DriverInformation as QDI
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import Tools.Notifications

softBlockNotifyDriver ::
  ( TranslateFlow m r,
    EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'SoftBlockNotifyDriver ->
  m ExecutionResult
softBlockNotifyDriver Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      driverId = jobData.driverId
      redisKey = jobData.pendingNotificationRedisKey
      entity = jobData.entityData
  driver <- BF.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  driverInfo <- QDI.findById driverId >>= fromMaybeM (PersonNotFound driverId.getId)
  if driverInfo.onRide
    then do
      now <- getCurrentTime
      let diffTime = diffUTCTime entity.blockExpirationTime now
      when (diffTime > 0) $ do
        let expiryTime = round (realToFrac diffTime :: Double)
        Redis.setExp redisKey entity expiryTime
      return $ Terminate "On ride so can't send for customer safety"
    else do
      notifySoftBlocked driver entity
      return Complete
