{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PayoutSplitConfig where

import qualified Domain.Types.PayoutSplitConfig
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PayoutSplitConfig as Beam

instance FromTType' Beam.PayoutSplitConfig Domain.Types.PayoutSplitConfig.PayoutSplitConfig where
  fromTType' (Beam.PayoutSplitConfigT {..}) = do
    pure $
      Just
        Domain.Types.PayoutSplitConfig.PayoutSplitConfig
          { area = area,
            bankDetails = bankDetails,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            vehicleVariant = vehicleVariant,
            vendorId = vendorId,
            vendorSplitAmount = vendorSplitAmount,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.PayoutSplitConfig Domain.Types.PayoutSplitConfig.PayoutSplitConfig where
  toTType' (Domain.Types.PayoutSplitConfig.PayoutSplitConfig {..}) = do
    Beam.PayoutSplitConfigT
      { Beam.area = area,
        Beam.bankDetails = bankDetails,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.vehicleVariant = vehicleVariant,
        Beam.vendorId = vendorId,
        Beam.vendorSplitAmount = vendorSplitAmount,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
