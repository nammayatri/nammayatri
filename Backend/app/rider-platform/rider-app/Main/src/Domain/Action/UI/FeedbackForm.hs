{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.FeedbackForm where

import Data.List (groupBy)
import Domain.Types.FeedbackForm
import Domain.Types.Person ()
import Domain.Types.Ride ()
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified Storage.CachedQueries.FeedbackForm as CQFF
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Ride as QRide

makeFeedbackFormList :: [FeedbackFormAPIEntity] -> FeedbackFormList
makeFeedbackFormList item =
  FeedbackFormList
    { _data = item
    }

makeFeedbackFormAPIEntity :: [FeedbackForm] -> [FeedbackFormAPIEntity]
makeFeedbackFormAPIEntity response = map convertGroup groupedEntities
  where
    groupedEntities = groupBy (\a b -> a.categoryName == b.categoryName) response
    convertGroup :: [FeedbackForm] -> FeedbackFormAPIEntity
    convertGroup [] = FeedbackFormAPIEntity {categoryName = RIDE, feedbackForm = []} -- should we throw error instead?
    convertGroup group@(res : _) =
      FeedbackFormAPIEntity
        { categoryName = res.categoryName,
          feedbackForm = map convertResponse group
        }
    convertResponse :: FeedbackForm -> FeedbackFormItem
    convertResponse res =
      FeedbackFormItem
        { id = res.id,
          rating = res.rating,
          question = res.question,
          answer = res.answer,
          answerType = res.answerType
        }

feedbackForm :: (CacheFlow m r, EsqDBFlow m r, HasCacheFeedbackFormConfig r) => Maybe Int -> m FeedbackFormList
feedbackForm ratingValue =
  do
    case ratingValue of
      Just rating -> do
        formList <- CQFF.findAllFeedbackByRating rating
        pure $ makeFeedbackFormList (makeFeedbackFormAPIEntity formList)
      Nothing -> do
        makeFeedbackFormList . makeFeedbackFormAPIEntity <$> CQFF.findAllFeedback

submitFeedback :: FeedbackFormReq -> Flow APISuccess
submitFeedback req = do
  let rideId = req.rideId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  CallBPPInternal.feedbackForm merchant.driverOfferBaseUrl req {rideId = cast ride.bppRideId}
