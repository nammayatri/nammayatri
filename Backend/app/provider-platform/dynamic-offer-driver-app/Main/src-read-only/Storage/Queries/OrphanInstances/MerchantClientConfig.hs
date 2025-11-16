{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantClientConfig where

import qualified Domain.Types.MerchantClientConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantClientConfig as Beam
import Storage.Queries.Transformers.MerchantClientConfig
import qualified Storage.Queries.Transformers.MerchantClientConfig

instance FromTType' Beam.MerchantClientConfig Domain.Types.MerchantClientConfig.MerchantClientConfig where
  fromTType' (Beam.MerchantClientConfigT {..}) = do
    clientServiceConfig' <- Storage.Queries.Transformers.MerchantClientConfig.mkServiceConfig configJSON serviceName
    pure $
      Just
        Domain.Types.MerchantClientConfig.MerchantClientConfig
          { clientOS = clientOS,
            clientServiceConfig = clientServiceConfig',
            createdAt = createdAt,
            packageId = packageId,
            updatedAt = updatedAt,
            merchantId = Kernel.Types.Id.Id <$> merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id <$> merchantOperatingCityId
          }

instance ToTType' Beam.MerchantClientConfig Domain.Types.MerchantClientConfig.MerchantClientConfig where
  toTType' (Domain.Types.MerchantClientConfig.MerchantClientConfig {..}) = do
    Beam.MerchantClientConfigT
      { Beam.clientOS = clientOS,
        Beam.configJSON = getConfigJSON clientServiceConfig,
        Beam.serviceName = getServiceName clientServiceConfig,
        Beam.createdAt = createdAt,
        Beam.packageId = packageId,
        Beam.updatedAt = updatedAt,
        Beam.merchantId = Kernel.Types.Id.getId <$> merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId <$> merchantOperatingCityId
      }
