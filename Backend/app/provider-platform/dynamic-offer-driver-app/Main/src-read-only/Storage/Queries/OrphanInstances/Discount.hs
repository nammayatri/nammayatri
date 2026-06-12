{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.Discount where

import qualified Domain.Types.Discount
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.Discount as Beam

instance FromTType' Beam.Discount Domain.Types.Discount.Discount where
  fromTType' (Beam.DiscountT {..}) = do
    pure $
      Just
        Domain.Types.Discount.Discount
          { config = config,
            discountType = discountType,
            enabled = enabled,
            id = Kernel.Types.Id.Id id,
            merchantId = Kernel.Types.Id.Id merchantId,
            merchantOperatingCityId = Kernel.Types.Id.Id merchantOperatingCityId,
            paymentMode = paymentMode,
            planId = Kernel.Types.Id.Id <$> planId,
            validFrom = validFrom,
            validTo = validTo,
            vehicleCategory = vehicleCategory,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.Discount Domain.Types.Discount.Discount where
  toTType' (Domain.Types.Discount.Discount {..}) = do
    Beam.DiscountT
      { Beam.config = config,
        Beam.discountType = discountType,
        Beam.enabled = enabled,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.merchantId = Kernel.Types.Id.getId merchantId,
        Beam.merchantOperatingCityId = Kernel.Types.Id.getId merchantOperatingCityId,
        Beam.paymentMode = paymentMode,
        Beam.planId = Kernel.Types.Id.getId <$> planId,
        Beam.validFrom = validFrom,
        Beam.validTo = validTo,
        Beam.vehicleCategory = vehicleCategory,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
