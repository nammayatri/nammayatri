{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Domain.Action.UI.FeedbackForm where

import qualified Data.HashMap.Strict as HM
import Data.List (groupBy, nub, nubBy, sortOn)
import qualified Domain.Types.Booking as DBooking
import Domain.Types.FeedbackForm
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified Domain.Types.Person as Person
import Environment
import Kernel.Prelude
import Kernel.Types.APISuccess
import Kernel.Types.Error
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified SharedLogic.CallBPPInternal as CallBPPInternal
import qualified SharedLogic.FeedbackForm as FFL
import qualified Storage.CachedQueries.FeedbackForm as CQFF
import qualified Storage.CachedQueries.Merchant as QMerchant
import qualified Storage.Queries.Booking as QBooking
import qualified Storage.Queries.Person as QPerson
import qualified Storage.Queries.QueriesExtra.BookingLite as QBookingLite
import qualified Storage.Queries.QueriesExtra.RideLite as QRideLite

makeFeedbackFormList :: [FeedbackFormAPIEntity] -> FeedbackFormList
makeFeedbackFormList item =
  FeedbackFormList
    { _data = item
    }

data CatalogueQuestion = CatalogueQuestion
  { cqKey :: Text,
    cqIsDefault :: Bool,
    cqRow :: FeedbackForm,
    cqPills :: [BadgeDetail]
  }

data CatalogueGroup = CatalogueGroup
  { cgRating :: Maybe Int,
    cgQuestions :: [CatalogueQuestion]
  }

isShownByDefault :: Maybe Bool -> Bool
isShownByDefault = fromMaybe False

mkCatalogue :: [FeedbackForm] -> [CatalogueGroup]
mkCatalogue rows = mapMaybe mkGroup ratingGroups
  where
    ratingGroups = groupBy (\a b -> a.rating == b.rating) $ sortOn (\f -> (f.rating, f.question)) rows

    mkGroup :: [FeedbackForm] -> Maybe CatalogueGroup
    mkGroup [] = Nothing
    mkGroup grp@(firstRow : _) =
      Just
        CatalogueGroup
          { cgRating = firstRow.rating,
            cgQuestions = map mkQuestion (firstRowPerKey grp)
          }

    -- One row is one question. questionKey is unique per city + rating; if two rows
    -- still share a key, the first one (in display order) wins.
    firstRowPerKey :: [FeedbackForm] -> [FeedbackForm]
    firstRowPerKey = nubBy (\a b -> rowQuestionKey a == rowQuestionKey b)

    mkQuestion :: FeedbackForm -> CatalogueQuestion
    mkQuestion row =
      CatalogueQuestion
        { cqKey = rowQuestionKey row,
          cqIsDefault = isShownByDefault row.shouldDefault,
          cqRow = row,
          cqPills = fromMaybe [] row.badges
        }

rowQuestionKey :: FeedbackForm -> Text
rowQuestionKey row = fromMaybe row.id.getId row.questionKey

defaultQuestions :: CatalogueGroup -> [CatalogueQuestion]
defaultQuestions grp = filter (.cqIsDefault) grp.cgQuestions

defaultPills :: CatalogueQuestion -> [BadgeDetail]
defaultPills q = filter (isShownByDefault . (.shouldDefault)) q.cqPills

-- | The form without any logic: shouldDefault questions with their shouldDefault pills.
renderDefault :: CatalogueGroup -> FeedbackFormAPIEntity
renderDefault grp =
  FeedbackFormAPIEntity
    { rating = grp.cgRating,
      questions = [renderQuestion q (defaultPills q) | q <- defaultQuestions grp]
    }

-- | The form a rule chose. The rule answers in keys, so each key is looked up in the
-- catalogue; unknown keys are dropped, and a question without an entry in `badges`
-- shows its default pills.
renderSelection :: CatalogueGroup -> FFL.FeedbackFormLogicOutput -> FeedbackFormAPIEntity
renderSelection grp selection =
  FeedbackFormAPIEntity
    { rating = grp.cgRating,
      questions = mapMaybe selectQuestion (nub selection.questions)
    }
  where
    selectQuestion :: Text -> Maybe FeedbackQuestion
    selectQuestion qKey = do
      q <- find ((== qKey) . (.cqKey)) grp.cgQuestions
      let pills = case selection.badges >>= HM.lookup qKey of
            Nothing -> defaultPills q
            Just pillKeys -> mapMaybe (\pKey -> find ((== pKey) . (.key)) q.cqPills) (nub pillKeys)
      pure $ renderQuestion q pills

renderQuestion :: CatalogueQuestion -> [BadgeDetail] -> FeedbackQuestion
renderQuestion q pills =
  FeedbackQuestion
    { questionId = q.cqRow.id.getId,
      question = q.cqRow.question,
      questionTranslations = q.cqRow.questionTranslations,
      badges = map badgeToBadgeItem pills
    }

badgeToBadgeItem :: BadgeDetail -> BadgeItem
badgeToBadgeItem badge =
  BadgeItem
    { key = badge.key,
      translations = badge.contentWithTranslations
    }

withoutEmptyGroups :: [FeedbackFormAPIEntity] -> [FeedbackFormAPIEntity]
withoutEmptyGroups = filter (not . null . (.questions))

feedbackForm :: Id Person.Person -> Maybe Int -> Maybe (Id DBooking.Booking) -> Flow FeedbackFormList
feedbackForm personId ratingValue mbBookingId = do
  person <- QPerson.findById personId >>= fromMaybeM (PersonNotFound personId.getId)
  let merchantOperatingCityId = person.merchantOperatingCityId
  formList <- case ratingValue of
    Just rating -> CQFF.findAllFeedbackByMerchantOpCityIdAndRating merchantOperatingCityId rating
    Nothing -> CQFF.findAllFeedbackByMerchantOpCityId merchantOperatingCityId
  let catalogue = mkCatalogue formList
  finalEntities <- case mbBookingId of
    Nothing -> pure $ map renderDefault catalogue
    Just bookingId -> applyFeedbackFormLogic personId merchantOperatingCityId bookingId catalogue
  pure $ makeFeedbackFormList (withoutEmptyGroups finalEntities)

applyFeedbackFormLogic :: Id Person.Person -> Id DMOC.MerchantOperatingCity -> Id DBooking.Booking -> [CatalogueGroup] -> Flow [FeedbackFormAPIEntity]
applyFeedbackFormLogic personId merchantOperatingCityId bookingId catalogue = do
  logics <- FFL.getFeedbackFormLogics merchantOperatingCityId
  if null logics
    then pure $ map renderDefault catalogue
    else do
      booking <- QBooking.findById bookingId >>= fromMaybeM (BookingNotFound bookingId.getId)
      unless (booking.riderId == personId) $ throwError (InvalidRequest "Booking does not belong to the person")
      forM catalogue $ \grp -> do
        let input =
              FFL.FeedbackFormLogicInput
                { rating = grp.cgRating,
                  serviceTier = booking.vehicleServiceTierType,
                  isAirConditioned = fromMaybe False booking.isAirConditioned,
                  specialLocationName = booking.specialLocationName,
                  questions = [q.cqKey | q <- defaultQuestions grp],
                  badges = HM.fromList [(q.cqKey, map (.key) (defaultPills q)) | q <- defaultQuestions grp]
                }
        mbSelection <- FFL.runFeedbackFormLogic logics input
        pure $ maybe (renderDefault grp) (renderSelection grp) mbSelection

submitFeedback :: FeedbackFormReq -> Flow APISuccess
submitFeedback req = do
  let rideId = req.rideId
  ride <- QRideLite.findByIdLite rideId >>= fromMaybeM (RideDoesNotExist rideId.getId)
  booking <- QBookingLite.findByIdLite ride.bookingId >>= fromMaybeM (BookingNotFound ride.bookingId.getId)
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
