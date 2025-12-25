{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.FeedbackForm where

import qualified Domain.Types.FeedbackForm
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FeedbackForm as Queries

findAllFeedback :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => m [Domain.Types.FeedbackForm.FeedbackForm]
findAllFeedback = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm")
    >>= ( \case
            Just a -> pure a
            Nothing -> (\dataToBeCached -> do expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime); Hedis.setExp "CachedQueries:FeedbackForm" dataToBeCached expTime) /=<< Queries.findAllFeedback
        )

findAllFeedbackByMerchantOpCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m [Domain.Types.FeedbackForm.FeedbackForm])
findAllFeedbackByMerchantOpCityId merchantOperatingCityId = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("CachedQueries:FeedbackForm:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId) dataToBeCached expTime
              )
                /=<< Queries.findAllFeedbackByMerchantOpCityId (Kernel.Prelude.Just merchantOperatingCityId)
        )

findAllFeedbackByMerchantOpCityIdAndRating ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> Kernel.Prelude.Int -> m [Domain.Types.FeedbackForm.FeedbackForm])
findAllFeedbackByMerchantOpCityIdAndRating merchantOperatingCityId rating = do
  (Hedis.safeGet $ "CachedQueries:FeedbackForm:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Rating-" <> show rating)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("CachedQueries:FeedbackForm:" <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId <> ":Rating-" <> show rating) dataToBeCached expTime
              )
                /=<< Queries.findAllFeedbackByMerchantOpCityIdAndRating (Kernel.Prelude.Just merchantOperatingCityId) (Kernel.Prelude.Just rating)
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
