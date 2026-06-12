{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.DiscountTier where

import qualified Domain.Types.Discount
import qualified Domain.Types.DiscountTier
import Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.DiscountTier as Queries

findAllByDiscountId :: (EsqDBFlow m r, MonadFlow m, CacheFlow m r) => (Kernel.Types.Id.Id Domain.Types.Discount.Discount -> m ([Domain.Types.DiscountTier.DiscountTier]))
findAllByDiscountId discountId = do
  (Hedis.safeGet $ "driverOfferCachedQueries:DiscountTier:" <> ":DiscountId-" <> Kernel.Types.Id.getId discountId)
    >>= ( \case
            Just a -> pure a
            Nothing ->
              ( \dataToBeCached -> do
                  expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                  Hedis.setExp ("driverOfferCachedQueries:DiscountTier:" <> ":DiscountId-" <> Kernel.Types.Id.getId discountId) dataToBeCached expTime
              )
                /=<< Queries.findAllByDiscountId discountId
        )
