module Storage.Queries.Rating where

import Beckn.Prelude
import Beckn.Storage.Esqueleto as Esq
import Beckn.Types.Id
import Beckn.Utils.Common
import Domain.Types.Person
import Domain.Types.Rating
import Domain.Types.Ride
import Storage.Tabular.Rating

create :: Rating -> SqlDB ()
create = Esq.create

updateRating :: Id Rating -> Id Person -> Int -> Maybe Text -> SqlDB ()
updateRating ratingId driverId newRatingValue feedbackDetails = do
  now <- getCurrentTime
  Esq.update $ \tbl -> do
    set
      tbl
      [ RatingRatingValue =. val newRatingValue,
        RatingFeedbackDetails =. val feedbackDetails,
        RatingUpdatedAt =. val now
      ]
    where_ $
      tbl ^. RatingTId ==. val (toKey ratingId)
        &&. tbl ^. RatingDriverId ==. val (toKey driverId)

findByRideId :: Transactionable m => Id Ride -> m (Maybe Rating)
findByRideId rideId =
  findOne $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingRideId ==. val (toKey rideId)
    return rating

findAllRatingsForPerson :: Transactionable m => Id Person -> m [Rating]
findAllRatingsForPerson driverId =
  findAll $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
    return rating
