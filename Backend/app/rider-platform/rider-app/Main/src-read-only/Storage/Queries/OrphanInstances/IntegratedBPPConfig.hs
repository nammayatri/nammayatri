{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.IntegratedBPPConfig where

import qualified Data.Aeson
import qualified Domain.Types.IntegratedBPPConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Kernel.Utils.JSON
import qualified Storage.Beam.IntegratedBPPConfig as Beam
import qualified Storage.Queries.Transformers.IntegratedBPPConfig

instance FromTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  fromTType' (Beam.IntegratedBPPConfigT {..}) = do
    providerConfig' <- Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfig configJSON
    pure $
      Just
        Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig
          { agencyKey = agencyKey,
            domain = domain,
            feedKey = feedKey,
            id = Kernel.Types.Id.Id id,
            isTicketValidOnMultipleRoutes = isTicketValidOnMultipleRoutes,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            ondcEncryptionPrivateKey = ondcEncryptionPrivateKey,
            ondcRegistryPublicKey = ondcRegistryPublicKey,
            operatorConfig = Kernel.Utils.JSON.valueToMaybe =<< operatorConfig,
            passOverrideApplicable = passOverrideApplicable,
            platformType = platformType,
            providerConfig = providerConfig',
            providerName = providerName,
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.IntegratedBPPConfig Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig where
  toTType' (Domain.Types.IntegratedBPPConfig.IntegratedBPPConfig {..}) = do
    Beam.IntegratedBPPConfigT
      { Beam.agencyKey = agencyKey,
        Beam.domain = domain,
        Beam.feedKey = feedKey,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.isTicketValidOnMultipleRoutes = isTicketValidOnMultipleRoutes,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.ondcEncryptionPrivateKey = ondcEncryptionPrivateKey,
        Beam.ondcRegistryPublicKey = ondcRegistryPublicKey,
        Beam.operatorConfig = Data.Aeson.toJSON <$> operatorConfig,
        Beam.passOverrideApplicable = passOverrideApplicable,
        Beam.platformType = platformType,
        Beam.configJSON = Storage.Queries.Transformers.IntegratedBPPConfig.getProviderConfigJson providerConfig,
        Beam.providerName = providerName,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
