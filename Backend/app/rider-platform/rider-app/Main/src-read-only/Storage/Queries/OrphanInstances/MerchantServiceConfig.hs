{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MerchantServiceConfig where

import qualified Data.Text
import qualified Domain.Types.MerchantServiceConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MerchantServiceConfig as Beam
import qualified Storage.Queries.Transformers.MerchantServiceConfig

instance FromTType' Beam.MerchantServiceConfig Domain.Types.MerchantServiceConfig.MerchantServiceConfig where
  fromTType' (Beam.MerchantServiceConfigT {..}) = do
    serviceConfig' <- Storage.Queries.Transformers.MerchantServiceConfig.getServiceConfigFromDomain serviceName configJSON
    pure $
      Just
        Domain.Types.MerchantServiceConfig.MerchantServiceConfig
          { createdAt = createdAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            runInCloud = (Kernel.Prelude.>>= (Kernel.Prelude.readMaybe . Data.Text.unpack)) runInCloud,
            serviceConfig = serviceConfig',
            updatedAt = updatedAt
          }

instance ToTType' Beam.MerchantServiceConfig Domain.Types.MerchantServiceConfig.MerchantServiceConfig where
  toTType' (Domain.Types.MerchantServiceConfig.MerchantServiceConfig {..}) = do
    Beam.MerchantServiceConfigT
      { Beam.createdAt = createdAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.runInCloud = Kernel.Prelude.fmap Kernel.Prelude.show runInCloud,
        Beam.configJSON = snd $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
        Beam.serviceName = fst $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
        Beam.updatedAt = updatedAt
      }
