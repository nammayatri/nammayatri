{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module SharedLogic.Allocator.Jobs.SendFeedbackPN
  ( sendFeedbackPN,
  )
where

import Kernel.Beam.Functions as BF
import Kernel.Prelude
import Kernel.Storage.Esqueleto.Config
import Kernel.Utils.Common
import Lib.Scheduler
import SharedLogic.Allocator (AllocatorJobType (..))
import qualified Storage.Queries.FeedbackBadgeExtra as QFeedbackBadge
import qualified Storage.Queries.Person as QPerson
import Tools.Error
import qualified Tools.Notifications as Notify

sendFeedbackPN ::
  ( EsqDBReplicaFlow m r,
    CacheFlow m r,
    EsqDBFlow m r
  ) =>
  Job 'SendFeedbackPN ->
  m ExecutionResult
sendFeedbackPN Job {id, jobInfo} = withLogTag ("JobId-" <> id.getId) do
  let jobData = jobInfo.jobData
      driverId = jobData.driverId
      badgeKey = jobData.badgeKey
      rating = jobData.rating
  driver <- BF.runInReplica $ QPerson.findById driverId >>= fromMaybeM (PersonDoesNotExist driverId.getId)
  badgeCount <- case badgeKey of
    Just key -> do
      mbBadge <- QFeedbackBadge.findFeedbackBadgeByKey driverId key
      pure $ (.badgeCount) <$> mbBadge
    Nothing -> pure Nothing
  let entityData =
        Notify.FeedbackBadgeEntityData
          { badgeKey = badgeKey,
            rating = rating,
            badgeCount = badgeCount
          }
  Notify.sendFeedbackBadgeNotification driver.merchantOperatingCityId driver entityData
  return Complete
