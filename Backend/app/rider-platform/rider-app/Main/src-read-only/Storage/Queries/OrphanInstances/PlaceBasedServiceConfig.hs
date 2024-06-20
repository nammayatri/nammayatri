{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PlaceBasedServiceConfig where

import qualified Domain.Types.PlaceBasedServiceConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PlaceBasedServiceConfig as Beam
import qualified Storage.Queries.Transformers.MerchantServiceConfig

instance FromTType' Beam.PlaceBasedServiceConfig Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig where
  fromTType' (Beam.PlaceBasedServiceConfigT {..}) = do
    serviceConfig' <- Storage.Queries.Transformers.MerchantServiceConfig.getServiceConfigFromDomain serviceName configValue
    pure $
      Just
        Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig
          { serviceConfig = serviceConfig',
            placeId = Kernel.Types.Id.Id placeId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            merchantId = Kernel.Types.Id.Id merchantId,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PlaceBasedServiceConfig Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig where
  toTType' (Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig {..}) = do
    Beam.PlaceBasedServiceConfigT
      { Beam.configValue = snd $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
        Beam.serviceName = fst $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
        Beam.placeId = Kernel.Types.Id.getId placeId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
