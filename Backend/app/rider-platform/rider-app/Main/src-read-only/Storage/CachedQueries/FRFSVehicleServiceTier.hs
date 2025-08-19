{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.CachedQueries.FRFSVehicleServiceTier where

import qualified BecknV2.FRFS.Enums
import qualified Domain.Types.FRFSVehicleServiceTier
import qualified Domain.Types.MerchantOperatingCity
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Storage.Hedis as Hedis
import qualified Kernel.Types.Id
import Kernel.Utils.Common
import qualified Storage.Queries.FRFSVehicleServiceTier as Queries

findByServiceTierAndMerchantOperatingCityId ::
  (EsqDBFlow m r, MonadFlow m, CacheFlow m r) =>
  (BecknV2.FRFS.Enums.ServiceTierType -> Kernel.Types.Id.Id Domain.Types.MerchantOperatingCity.MerchantOperatingCity -> m (Kernel.Prelude.Maybe Domain.Types.FRFSVehicleServiceTier.FRFSVehicleServiceTier))
findByServiceTierAndMerchantOperatingCityId _type merchantOperatingCityId = do
  (Hedis.safeGet $ "CachedQueries:FRFSVehicleServiceTier:" <> ":_type-" <> show _type <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId)
    >>= ( \case
            Just a -> pure (Just a)
            Nothing ->
              flip
                whenJust
                ( \dataToBeCached -> do
                    expTime <- fromIntegral <$> asks (.cacheConfig.configsExpTime)
                    Hedis.setExp ("CachedQueries:FRFSVehicleServiceTier:" <> ":_type-" <> show _type <> ":MerchantOperatingCityId-" <> Kernel.Types.Id.getId merchantOperatingCityId) dataToBeCached expTime
                )
                /=<< Queries.findByServiceTierAndMerchantOperatingCityId _type merchantOperatingCityId
        )
