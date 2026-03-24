{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}


module Storage.Queries.OrphanInstances.PlaceBasedServiceConfig where
import Kernel.Beam.Functions
import Kernel.Prelude
import Kernel.External.Encryption
import Kernel.Utils.Common (MonadFlow, CacheFlow, EsqDBFlow, getCurrentTime, fromMaybeM)
import Kernel.Types.Error
import qualified Domain.Types.PlaceBasedServiceConfig
import qualified Storage.Beam.PlaceBasedServiceConfig as Beam
import qualified Kernel.Types.Id
import qualified Storage.Queries.Transformers.MerchantServiceConfig



instance FromTType' Beam.PlaceBasedServiceConfig Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig
    where fromTType' (Beam.PlaceBasedServiceConfigT {..}) = do {serviceConfig' <- Storage.Queries.Transformers.MerchantServiceConfig.getServiceConfigFromDomain serviceName configValue;
                                                                pure $ Just Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig{merchantId = Kernel.Types.Id.Id merchantId,
                                                                                                                                         merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
                                                                                                                                         placeId = Kernel.Types.Id.Id placeId,
                                                                                                                                         serviceConfig = serviceConfig',
                                                                                                                                         createdAt = createdAt,
                                                                                                                                         updatedAt = updatedAt}}
instance ToTType' Beam.PlaceBasedServiceConfig Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig
    where toTType' (Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig {..}) = do Beam.PlaceBasedServiceConfigT{Beam.merchantId = Kernel.Types.Id.getId merchantId,
                                                                                                                          Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
                                                                                                                          Beam.placeId = Kernel.Types.Id.getId placeId,
                                                                                                                          Beam.configValue = snd $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
                                                                                                                          Beam.serviceName = fst $ Storage.Queries.Transformers.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
                                                                                                                          Beam.createdAt = createdAt,
                                                                                                                          Beam.updatedAt = updatedAt}



