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
import qualified Domain.Types.MerchantOperatingCity as DMOC
import qualified IssueManagement.Common as IMC
import Kernel.External.Types (Language)
import Kernel.Prelude
import Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.CachedQueries.FeedbackForm as CQFF

filterTranslationsByLanguage :: Language -> Maybe [IMC.Translation] -> Maybe [IMC.Translation]
filterTranslationsByLanguage lang mbTranslations =
  case mbTranslations of
    Nothing -> Nothing
    Just translations ->
      let filtered = filter (\t -> t.language == lang) translations
       in if null filtered then Nothing else Just filtered

filterFeedbackFormByLanguage :: Language -> FeedbackForm -> FeedbackForm
filterFeedbackFormByLanguage lang form =
  form
    { questionTranslations = filterTranslationsByLanguage lang form.questionTranslations,
      badges = fmap (map (filterBadgeTranslations lang)) form.badges
    }

filterBadgeTranslations :: Language -> BadgeDetail -> BadgeDetail
filterBadgeTranslations lang badge =
  badge {contentWithTranslations = filterTranslationsByLanguage lang badge.contentWithTranslations}

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
          questions = mapMaybe convertQuestionGroup (groupBy (\a b -> a.question == b.question) group)
        }

    convertQuestionGroup :: [FeedbackForm] -> Maybe FeedbackQuestion
    convertQuestionGroup [] = Nothing
    convertQuestionGroup group@(res : _) =
      Just
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

feedbackForm ::
  (CacheFlow m r, EsqDBFlow m r) =>
  Id DMOC.MerchantOperatingCity ->
  Language ->
  Maybe Int ->
  m FeedbackFormList
feedbackForm merchantOperatingCityId language ratingValue = do
  formList <- case ratingValue of
    Just rating -> CQFF.findAllFeedbackByMerchantOpCityIdAndRating merchantOperatingCityId rating
    Nothing -> CQFF.findAllFeedbackByMerchantOpCityId merchantOperatingCityId

  let filteredForms = map (filterFeedbackFormByLanguage language) formList
  pure $ makeFeedbackFormList (makeFeedbackFormAPIEntity filteredForms)
