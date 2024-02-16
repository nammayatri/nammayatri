{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}
{-# OPTIONS_GHC -Wno-orphans #-}

module Storage.Queries.Rating where

import Domain.Types.Person
import Domain.Types.Rating as DR
import Domain.Types.Ride
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.Types.Id (Id (..))
import Kernel.Utils.Common
import qualified Sequelize as Se
import qualified Storage.Beam.Rating as BeamR

create :: MonadFlow m => DR.Rating -> m ()
create = createWithKV

updateRating :: MonadFlow m => Id Rating -> Id Person -> Int -> Maybe Text -> Maybe Bool -> m ()
updateRating (Id ratingId) (Id riderId) newRatingValue newFeedbackDetails wasOfferedAssistance = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.ratingValue newRatingValue,
      Se.Set BeamR.feedbackDetails newFeedbackDetails,
      Se.Set BeamR.wasOfferedAssistance wasOfferedAssistance,
      Se.Set BeamR.updatedAt now
    ]
    [Se.And [Se.Is BeamR.id (Se.Eq ratingId), Se.Is BeamR.riderId (Se.Eq riderId)]]

findAllRatingsForPerson :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Person -> m [Rating]
findAllRatingsForPerson riderId = findAllWithDb [Se.Is BeamR.riderId $ Se.Eq $ getId riderId]

findRatingForRide :: (MonadFlow m, CacheFlow m r, EsqDBFlow m r) => Id Ride -> m (Maybe Rating)
findRatingForRide (Id rideId) = findOneWithKV [Se.Is BeamR.rideId $ Se.Eq rideId]

-- findAllRatingUsersCountByPerson :: (L.MonadFlow m, Log m) => Id Person -> m Int
-- findAllRatingUsersCountByPerson (Id riderId) = findAllWithKV [Se.Is BeamR.riderId $ Se.Eq riderId] <&> length

instance FromTType' BeamR.Rating Rating where
  fromTType' BeamR.RatingT {..} = do
    pure $
      Just
        Rating
          { id = Id id,
            rideId = Id rideId,
            riderId = Id riderId,
            ratingValue = ratingValue,
            feedbackDetails = feedbackDetails,
            wasOfferedAssistance = wasOfferedAssistance,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamR.Rating Rating where
  toTType' Rating {..} = do
    BeamR.RatingT
      { BeamR.id = getId id,
        BeamR.rideId = getId rideId,
        BeamR.riderId = getId riderId,
        BeamR.ratingValue = ratingValue,
        BeamR.feedbackDetails = feedbackDetails,
        BeamR.wasOfferedAssistance = wasOfferedAssistance,
        BeamR.createdAt = createdAt,
        BeamR.updatedAt = updatedAt
      }
