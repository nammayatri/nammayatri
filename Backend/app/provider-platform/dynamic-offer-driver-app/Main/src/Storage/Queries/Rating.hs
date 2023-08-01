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
import qualified EulerHS.Language as L
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import Lib.Utils (FromTType' (fromTType'), ToTType' (toTType'), createWithKV, findAllWithKV, findOneWithKV, findOneWithKvInReplica, updateOneWithKV)
import qualified Sequelize as Se
import qualified Storage.Beam.Rating as BeamR

create :: (L.MonadFlow m, Log m) => DR.Rating -> m ()
create = createWithKV

updateRating :: (L.MonadFlow m, MonadTime m, Log m) => Id Rating -> Id Person -> Int -> Maybe Text -> m ()
updateRating (Id ratingId) (Id driverId) newRatingValue newFeedbackDetails = do
  now <- getCurrentTime
  updateOneWithKV
    [ Se.Set BeamR.ratingValue newRatingValue,
      Se.Set BeamR.feedbackDetails newFeedbackDetails,
      Se.Set BeamR.updatedAt now
    ]
    [Se.And [Se.Is BeamR.id (Se.Eq ratingId), Se.Is BeamR.driverId (Se.Eq driverId)]]

findAllRatingsForPerson :: (L.MonadFlow m, Log m) => Id Person -> m [Rating]
findAllRatingsForPerson driverId = findAllWithKV [Se.Is BeamR.driverId $ Se.Eq $ getId driverId]

findRatingForRide :: (L.MonadFlow m, Log m) => Id Ride -> m (Maybe Rating)
findRatingForRide (Id rideId) = findOneWithKV [Se.Is BeamR.id $ Se.Eq rideId]

findRatingForRideInReplica :: (L.MonadFlow m, Log m) => Id Ride -> m (Maybe Rating)
findRatingForRideInReplica (Id rideId) = findOneWithKvInReplica [Se.Is BeamR.id $ Se.Eq rideId]

instance FromTType' BeamR.Rating Rating where
  fromTType' BeamR.RatingT {..} = do
    pure $
      Just
        Rating
          { id = Id id,
            rideId = Id rideId,
            driverId = Id driverId,
            ratingValue = ratingValue,
            feedbackDetails = feedbackDetails,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' BeamR.Rating Rating where
  toTType' Rating {..} = do
    BeamR.RatingT
      { BeamR.id = getId id,
        BeamR.rideId = getId rideId,
        BeamR.driverId = getId driverId,
        BeamR.ratingValue = ratingValue,
        BeamR.feedbackDetails = feedbackDetails,
        BeamR.createdAt = createdAt,
        BeamR.updatedAt = updatedAt
      }

findAllRatingUsersCountByPerson :: Transactionable m => Id Person -> m Int
findAllRatingUsersCountByPerson driverId =
  mkCount <$> do
    findAll $ do
      rating <- from $ table @RatingT
      where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
      return (countRows :: SqlExpr (Esq.Value Int))
  where
    mkCount [counter] = counter
    mkCount _ = 0
