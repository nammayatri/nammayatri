{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.DiscountTranslation where

import qualified Domain.Types.Discount
import qualified Domain.Types.DiscountTranslation
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DiscountTranslation as Queries

findByDiscountIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> Kernel.External.Types.Language -> m (Kernel.Prelude.Maybe Domain.Types.DiscountTranslation.DiscountTranslation))
findByDiscountIdAndLanguage discountId language = do
  (Hedis.safeGet $ "driverOfferCachedQueries:DiscountTranslation:" <> ":DiscountId-" <> Kernel.Types.Id.getId discountId <> ":Language-" <> show language)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("driverOfferCachedQueries:DiscountTranslation:" <> ":DiscountId-" <> Kernel.Types.Id.getId discountId <> ":Language-" <> show language) dataToBeCached expTime
                )
                /=<< Queries.findByDiscountIdAndLanguage discountId language
        )
