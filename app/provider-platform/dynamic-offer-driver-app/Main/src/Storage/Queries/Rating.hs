module Storage.Queries.Rating where

import Domain.Types.Person
import Domain.Types.Rating
import Kernel.Prelude
import Kernel.Storage.Esqueleto as Esq
import Kernel.Types.Id
import Kernel.Utils.Common
import Storage.Tabular.Rating

create :: Rating -> SqlDB ()
create = Esq.create

updateRating :: Id Rating -> Id Person -> Int -> Maybe Text -> SqlDB ()
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

findAllRatingsForPerson :: Transactionable m => Id Person -> m [Rating]
findAllRatingsForPerson driverId =
  findAll $ do
    rating <- from $ table @RatingT
    where_ $ rating ^. RatingDriverId ==. val (toKey driverId)
    return rating
