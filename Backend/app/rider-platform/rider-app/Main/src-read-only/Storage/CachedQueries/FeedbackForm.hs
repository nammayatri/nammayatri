{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.FeedbackForm where

import qualified Domain.Types.FeedbackForm
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import Kernel.Utils.Common
import qualified Storage.Queries.FeedbackForm as Queries

findAllFeedback :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m [Domain.Types.FeedbackForm.FeedbackForm]
findAllFeedback = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm")
    >>= ( \case
            Just a -> pure a
            Nothing -> (\dataToBeCached -> do expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime); Hedis.setExp "CachedQueries:FeedbackForm" dataToBeCached expTime) /=<< Queries.findAllFeedback
        )

findAllFeedbackByRating :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Prelude.Int -> m [Domain.Types.FeedbackForm.FeedbackForm])
findAllFeedbackByRating rating = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm:" <> ":Rating-" <> show rating)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("CachedQueries:FeedbackForm:" <> ":Rating-" <> show rating) dataToBeCached expTime
              )
                /=<< Queries.findAllFeedbackByRating rating
        )

{-
	DSL Source Link: file://./../../../spec/Storage/FeedbackForm.yaml
-}
