{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantServiceConfig where

import qualified Domain.Types.MerchantServiceConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantServiceConfig as Beam
import Storage.Queries.Transformers.MerchantServiceConfig
import qualified Storage.Queries.Transformers.MerchantServiceConfig

instance FromTType' Beam.MerchantServiceConfig Domain.Types.MerchantServiceConfig.MerchantServiceConfig where
  fromTType' (Beam.MerchantServiceConfigT {..}) = do
    serviceConfig' <- Storage.Queries.Transformers.MerchantServiceConfig.mkServiceConfig configJSON serviceName
    pure $
      Just
        Domain.Types.MerchantServiceConfig.MerchantServiceConfig
          { createdAt = createdAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId,
            serviceConfig = serviceConfig',
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantServiceConfig Domain.Types.MerchantServiceConfig.MerchantServiceConfig where
  toTType' (Domain.Types.MerchantServiceConfig.MerchantServiceConfig {..}) = do
    Beam.MerchantServiceConfigT
      { Beam.createdAt = createdAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId,
        Beam.configJSON = getConfigJSON serviceConfig,
        Beam.serviceName = getServiceName serviceConfig,
        Beam.updatedAt = updatedAt
      }
