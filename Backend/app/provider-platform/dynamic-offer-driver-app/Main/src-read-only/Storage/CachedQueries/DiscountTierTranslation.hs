{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.DiscountTierTranslation where

import qualified Domain.Types.DiscountTier
import qualified Domain.Types.DiscountTierTranslation
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DiscountTierTranslation as Queries

findByTierIdAndLanguage ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (Kernel.Types.Id.Id Domain.Types.DiscountTier.DiscountTier -> Kernel.External.Types.Language -> m (Kernel.Prelude.Maybe Domain.Types.DiscountTierTranslation.DiscountTierTranslation))
findByTierIdAndLanguage tierId language = do
  (Hedis.safeGet $ "driverOfferCachedQueries:DiscountTierTranslation:" <> ":TierId-" <> Kernel.Types.Id.getId tierId <> ":Language-" <> show language)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("driverOfferCachedQueries:DiscountTierTranslation:" <> ":TierId-" <> Kernel.Types.Id.getId tierId <> ":Language-" <> show language) dataToBeCached expTime
                )
                /=<< Queries.findByTierIdAndLanguage tierId language
        )
