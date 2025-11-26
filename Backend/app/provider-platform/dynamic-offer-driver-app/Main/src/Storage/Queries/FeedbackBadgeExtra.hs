{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.FeedbackBadgeExtra where

import qualified Domain.Types.FeedbackBadge as Domain
import qualified Domain.Types.Person as Person
import Kernel.Beam.Functions
import Kernel.Prelude
import qualified Kernel.Types.Id as Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.FeedbackBadge as Beam
import Storage.Queries.OrphanInstances.FeedbackBadge ()

findFeedbackBadgeForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Text -> m (Maybe Domain.FeedbackBadge)
findFeedbackBadgeForDriver driverId badge =
  findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Id.getId driverId), Se.Is Beam.badge $ Se.Eq badge]]

findFeedbackBadgeByKey :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id.Id Person.Person -> Text -> m (Maybe Domain.FeedbackBadge)
findFeedbackBadgeByKey driverId badgeKey =
  findOneWithKV [Se.And [Se.Is Beam.driverId $ Se.Eq (Id.getId driverId), Se.Is Beam.badgeKey $ Se.Eq (Just badgeKey)]]

updateFeedbackBadge :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Domain.FeedbackBadge -> Int -> m ()
updateFeedbackBadge feedbackBadge newBadgeCount = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set Beam.badgeCount newBadgeCount,
      Se.Set Beam.updatedAt now
    ]
    [Se.And [Se.Is Beam.id $ Se.Eq (Id.getId feedbackBadge.id), Se.Is Beam.driverId $ Se.Eq (Id.getId feedbackBadge.driverId)]]
