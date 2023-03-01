{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.Queries.Rating where

import Domain.Types.Person
import Domain.Types.Rating
import Domain.Types.Ride
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Rating

create :: Rating -> SqlDB m ()
create = Esq.create

updateRating :: Id Rating -> Id Person -> Int -> Maybe Text -> SqlDB m ()
updateRating ratingId driverId newRatingValue newFeedbackDetails = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RatingRatingValue =. val newRatingValue,
        RatingFeedbackDetails =. val newFeedbackDetails,
        RatingUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RatingTId ==. val (toKey ratingId)
        &&. tbl ^. RatingDriverId ==. val (toKey driverId)

findAllRatingsForPerson :: forall m ma. Transactionable ma m => Id Person -> Proxy ma -> m [Rating]
findAllRatingsForPerson driverId _ =
  findAll @m @ma $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
    return rating

findRatingForRide :: forall m ma. Transactionable ma m => Id Ride -> Proxy ma -> m (Maybe Rating)
findRatingForRide rideId _ = findOne @m @ma $ do
  rating <- from $ table @RatingT
  where_ $ rating ^. RatingRideId ==. val (toKey rideId)
  pure rating
