{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.FeedbackForm where

import Data.List (groupBy, sortOn)
import Domain.Types.FeedbackForm
import qualified Domain.Types.Person as Person
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
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.Ride as QRide

makeFeedbackFormList :: [FeedbackFormAPIEntity] -> FeedbackFormList
makeFeedbackFormList item =
  FeedbackFormList
    { _data = item
    }

makeFeedbackFormAPIEntity :: [FeedbackForm] -> [FeedbackFormAPIEntity]
makeFeedbackFormAPIEntity response = map convertRatingGroup ratingGroups
  where
    sortedResponse = sortOn (\f -> (f.rating, f.question)) response

    ratingGroups = groupBy (\a b -> a.rating == b.rating) sortedResponse

    convertRatingGroup :: [FeedbackForm] -> FeedbackFormAPIEntity
    convertRatingGroup [] = FeedbackFormAPIEntity {rating = Nothing, questions = []}
    convertRatingGroup group@(res : _) =
      FeedbackFormAPIEntity
        { rating = res.rating,
          questions = map convertQuestionGroup (groupBy (\a b -> a.question == b.question) group)
        }

    convertQuestionGroup :: [FeedbackForm] -> FeedbackQuestion
    convertQuestionGroup [] = FeedbackQuestion {questionId = "", question = "", questionTranslations = Nothing, badges = []}
    convertQuestionGroup group@(res : _) =
      FeedbackQuestion
        { questionId = res.id.getId,
          question = res.question,
          questionTranslations = res.questionTranslations,
          badges = concatMap extractBadges group
        }

    extractBadges :: FeedbackForm -> [BadgeItem]
    extractBadges form =
      case form.badges of
        Nothing -> []
        Just badgeList -> map badgeToBadgeItem badgeList

    badgeToBadgeItem :: BadgeDetail -> BadgeItem
    badgeToBadgeItem badge =
      BadgeItem
        { key = badge.key,
          translations = badge.contentWithTranslations
        }

feedbackForm :: (CacheFlow m r, EsqDBFlow m r, HasCacheFeedbackFormConfig r) => Id Person.Person -> Maybe Int -> m FeedbackFormList
feedbackForm personId ratingValue = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  formList <- case ratingValue of
    Just rating -> CQFF.findAllFeedbackByMerchantOpCityIdAndRating merchantOperatingCityId rating
    Nothing -> CQFF.findAllFeedbackByMerchantOpCityId merchantOperatingCityId

  pure $ makeFeedbackFormList (makeFeedbackFormAPIEntity formList)

submitFeedback :: FeedbackFormReq -> Flow APISuccess
submitFeedback req = do
  let rideId = req.rideId
  ride <- QRide.findById rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBooking.findById ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
  merchant <- QMerchant.findById booking.merchantId >>= fromMaybeM (MerchantNotFound booking.merchantId.getId)
  let bppReq =
        CallBPPInternal.FeedbackFormReq
          { rideId = ride.bppRideId.getId,
            rating = Nothing,
            feedbackDetails = Nothing,
            badges = Nothing,
            feedback = req.feedback
          }
  CallBPPInternal.feedbackForm merchant.driverOfferBaseUrl bppReq
