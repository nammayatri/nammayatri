{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.MerchantServiceConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.MerchantServiceConfig
import qualified Storage.Beam.MerchantServiceConfig as Beam
import qualified Kernel.Types.Id
import qualified Storage.Queries.Transformers.MerchantServiceConfig



instance FromTType' Beam.MerchantServiceConfig Domain.Types.MerchantServiceConfig.MerchantServiceConfig
    where fromTType' (Beam.MerchantServiceConfigT {..}) = do {serviceConfig' <- Storage.Queries.Transformers.MerchantServiceConfig.getServiceConfigFromDomain serviceName configJSON;
                                                              pure $ Just Domain.Types.MerchantServiceConfig.MerchantServiceConfig{createdAt = createdAt,
                                                                                                                                   merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                   merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                   serviceConfig = serviceConfig',
                                                                                                                                   updatedAt = updatedAt}}
instance ToTType' Beam.MerchantServiceConfig Domain.Types.MerchantServiceConfig.MerchantServiceConfig
    where toTType' (Domain.Types.MerchantServiceConfig.MerchantServiceConfig {..}) = do Beam.MerchantServiceConfigT{Beam.createdAt = createdAt,
                                                                                                                    Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                    Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                    Beam.configJSON = (snd $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig),
                                                                                                                    Beam.serviceName = (fst $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig),
                                                                                                                    Beam.updatedAt = updatedAt}



