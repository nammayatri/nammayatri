{-
 Copyright 2022-23, Juspay India Pvt Ltd

 This program is free software: you can redistribute it and/or modify it under the terms of the GNU Affero General Public License

 as published by the Free Software Foundation, either version 3 of the License, or (at your option) any later version. This program

 is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY

 or FITNESS FOR A PARTICULAR PURPOSE. See the GNU Affero General Public License for more details. You should have received a copy of

 the GNU Affero General Public License along with this program. If not, see <https://www.gnu.org/licenses/>.
-}

module Storage.CachedQueries.FeedbackForm where

import Domain.Types.FeedbackForm
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Types.Common
import Storage.Queries.FeedbackForm as Queries

findAllFeedback :: (Hedis.CacheFlow m r, EsqDBFlow m r, HasCacheFeedbackFormConfig r) => m [FeedbackFormRes]
findAllFeedback =
  Hedis.safeGet "CachedQueries:FeedbackForm" >>= \case
    Just a -> return a
    Nothing -> cacheFeedback "CachedQueries:FeedbackForm" /=<< Queries.findAllFeedback

findAllFeedbackByRating :: (Hedis.CacheFlow m r, EsqDBFlow m r, HasCacheFeedbackFormConfig r) => Int -> m [FeedbackFormRes]
findAllFeedbackByRating rating =
  Hedis.safeGet (makefeedbackRatingKey rating) >>= \case
    Just a -> return a
    Nothing -> cacheFeedback (makefeedbackRatingKey rating) /=<< Queries.findAllFeedbackByRating rating

makefeedbackRatingKey :: Int -> Text
makefeedbackRatingKey rating = "CachedQueries:FeedbackForm" <> ":Rating" <> show rating

cacheFeedback :: (Hedis.CacheFlow m r, HasCacheFeedbackFormConfig r) => Text -> [FeedbackFormRes] -> m ()
cacheFeedback key feedbacks = do
  expTime <- fromIntegral <$> asks (.cacheFeedbackFormConfig.configsExpTime)
  Hedis.setExp key feedbacks expTime
