{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.MonetaryRewardConfig where

import qualified Domain.Types.MonetaryRewardConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.MonetaryRewardConfig as Beam

instance FromTType' Beam.MonetaryRewardConfig Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig where
  fromTType' (Beam.MonetaryRewardConfigT {..}) = do
    pure $
      Just
        Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig
          { active = active,
            eventFunction = eventFunction,
            eventName = eventName,
            expirationAt = expirationAt,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            monetaryRewardAmount = monetaryRewardAmount,
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.MonetaryRewardConfig Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig where
  toTType' (Domain.Types.MonetaryRewardConfig.MonetaryRewardConfig {..}) = do
    Beam.MonetaryRewardConfigT
      { Beam.active = active,
        Beam.eventFunction = eventFunction,
        Beam.eventName = eventName,
        Beam.expirationAt = expirationAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.monetaryRewardAmount = monetaryRewardAmount,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
