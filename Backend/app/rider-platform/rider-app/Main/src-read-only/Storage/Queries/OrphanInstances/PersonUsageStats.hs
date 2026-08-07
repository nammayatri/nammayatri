{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.PersonUsageStats where

import qualified Domain.Types.PersonUsageStats
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.PersonUsageStats as Beam

instance FromTType' Beam.PersonUsageStats Domain.Types.PersonUsageStats.PersonUsageStats where
  fromTType' (Beam.PersonUsageStatsT {..}) = do
    pure $
      Just
        Domain.Types.PersonUsageStats.PersonUsageStats
          { createdAt = createdAt,
            id = Kernel.Types.Id.Id id,
            lastPurchasedAt = lastPurchasedAt,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            passTypeId = Kernel.Types.Id.Id <$> passTypeId,
            personId = Kernel.Types.Id.Id personId,
            productType = productType,
            purchaseCount = purchaseCount,
            staticPersonId = staticPersonId,
            ticketCount = ticketCount,
            updatedAt = updatedAt,
            vehicleServiceTierType = vehicleServiceTierType,
            vehicleType = vehicleType
          }

instance ToTType' Beam.PersonUsageStats Domain.Types.PersonUsageStats.PersonUsageStats where
  toTType' (Domain.Types.PersonUsageStats.PersonUsageStats {..}) = do
    Beam.PersonUsageStatsT
      { Beam.createdAt = createdAt,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.lastPurchasedAt = lastPurchasedAt,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.passTypeId = Kernel.Types.Id.getId <$> passTypeId,
        Beam.personId = Kernel.Types.Id.getId personId,
        Beam.productType = productType,
        Beam.purchaseCount = purchaseCount,
        Beam.staticPersonId = staticPersonId,
        Beam.ticketCount = ticketCount,
        Beam.updatedAt = updatedAt,
        Beam.vehicleServiceTierType = vehicleServiceTierType,
        Beam.vehicleType = vehicleType
      }
