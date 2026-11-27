{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.AiVerificationQuestion where

import qualified Domain.Types.AiVerificationQuestion
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.AiVerificationQuestion as Queries

findAllByMerchantIdAndEntityType ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Merchant.Merchant -> Kernel.Prelude.Text -> m ([Domain.Types.AiVerificationQuestion.AiVerificationQuestion]))
findAllByMerchantIdAndEntityType merchantId entityType = do
  (Hedis.safeGet $ "driverOfferCachedQueries:AiVerificationQuestion:" <> ":MerchantId-" <> Kernel.Types.Id.getId merchantId <> ":EntityType-" <> show entityType)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("driverOfferCachedQueries:AiVerificationQuestion:" <> ":MerchantId-" <> Kernel.Types.Id.getId merchantId <> ":EntityType-" <> show entityType) dataToBeCached expTime
              )
                /=<< Queries.findAllByMerchantIdAndEntityType merchantId entityType
        )
