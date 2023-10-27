{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Feedback.FeedbackBadge where

import Data.Text
import Domain.Types.Feedback.Feedback
import Domain.Types.Person (Person)
import Kernel.Beam.Functions
import Kernel.Types.Id
import Kernel.Types.Time
import Kernel.Utils.Common
import Sequelize as Se
import qualified Storage.Beam.Feedback.FeedbackBadge as BFFB
import Prelude hiding (id)

createFeedbackBadge :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FeedbackBadge -> m ()
createFeedbackBadge = createWithKV

findFeedbackBadgeForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> Text -> m (Maybe FeedbackBadge)
findFeedbackBadgeForDriver (Id driverId) badge = findOneWithKV [Se.And [Se.Is BFFB.driverId $ Se.Eq driverId, Se.Is BFFB.badge $ Se.Eq badge]]

updateFeedbackBadge :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => FeedbackBadge -> Int -> m ()
updateFeedbackBadge feedbackBadge newBadgeCount = do
  now <- getCurrentTime
  updateWithKV
    [ Se.Set BFFB.badgeCount newBadgeCount,
      Se.Set BFFB.updatedAt now
    ]
    [Se.And [Se.Is BFFB.id $ Se.Eq $ getId feedbackBadge.id, Se.Is BFFB.driverId $ Se.Eq $ getId feedbackBadge.driverId]]

findAllFeedbackBadgeForDriver :: (MonadFlow m, EsqDBFlow m r, CacheFlow m r) => Id Person -> m [FeedbackBadge]
findAllFeedbackBadgeForDriver (Id driverId) = findAllWithKV [Se.Is BFFB.driverId $ Se.Eq driverId]

instance FromTType' BFFB.FeedbackBadge FeedbackBadge where
  fromTType' BFFB.FeedbackBadgeT {..} = do
    pure $
      Just
        FeedbackBadge
          { id = Id id,
            driverId = Id driverId,
            badge = badge,
            badgeCount = badgeCount,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BFFB.FeedbackBadge FeedbackBadge where
  toTType' FeedbackBadge {..} =
    BFFB.FeedbackBadgeT
      { BFFB.id = getId id,
        BFFB.driverId = getId driverId,
        BFFB.badge = badge,
        BFFB.badgeCount = badgeCount,
        BFFB.createdAt = createdAt,
        BFFB.updatedAt = updatedAt
      }
