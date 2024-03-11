{-# OPTIONS_GHC -Wno-deprecations #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.PlaceBasedServiceConfig where

import qualified Domain.Types.Merchant
import qualified Domain.Types.Merchant.MerchantServiceConfig
import qualified Domain.Types.MerchantOperatingCity
import qualified Domain.Types.PlaceBasedServiceConfig
import qualified Domain.Types.TicketPlace
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, KvDbFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Sequelize as Se
import qualified Storage.Beam.PlaceBasedServiceConfig as Beam
import qualified Storage.Queries.Merchant.MerchantServiceConfig

findByPlaceIdAndServiceName :: KvDbFlow m r => Kernel.Types.Id.Id Domain.Types.TicketPlace.TicketPlace -> Domain.Types.Merchant.MerchantServiceConfig.ServiceName -> m (Maybe (Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig))
findByPlaceIdAndServiceName (Kernel.Types.Id.Id placeId) serviceName = do
  findOneWithKV
    [ Se.And
        [ Se.Is Beam.placeId $ Se.Eq placeId,
          Se.Is Beam.serviceName $ Se.Eq serviceName
        ]
    ]

instance FromTType' Beam.PlaceBasedServiceConfig Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig where
  fromTType' Beam.PlaceBasedServiceConfigT {..} = do
    serviceConfig <- Storage.Queries.Merchant.MerchantServiceConfig.getServiceConfigFromDomain serviceName configValue
    pure $
      Just
        Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig
          { merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            placeId = Kernel.Types.Id.Id placeId,
            serviceConfig = serviceConfig,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PlaceBasedServiceConfig Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig where
  toTType' Domain.Types.PlaceBasedServiceConfig.PlaceBasedServiceConfig {..} = do
    Beam.PlaceBasedServiceConfigT
      { Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.placeId = Kernel.Types.Id.getId placeId,
        Beam.serviceName = fst $ Storage.Queries.Merchant.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
        Beam.configValue = snd $ Storage.Queries.Merchant.MerchantServiceConfig.getServiceNameConfigJson serviceConfig,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
