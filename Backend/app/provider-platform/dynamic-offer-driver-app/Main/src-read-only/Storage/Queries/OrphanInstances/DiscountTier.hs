{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.DiscountTier where

import qualified Domain.Types.DiscountTier
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import qualified Kernel.Types.Id
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.DiscountTier as Beam

instance FromTType' Beam.DiscountTier Domain.Types.DiscountTier.DiscountTier where
  fromTType' (Beam.DiscountTierT {..}) = do
    pure $
      Just
        Domain.Types.DiscountTier.DiscountTier
          { discountId = Kernel.Types.Id.Id discountId,
            discountValue = discountValue,
            discountValueType = discountValueType,
            id = Kernel.Types.Id.Id id,
            thresholdValue = thresholdValue,
            tierOrder = tierOrder,
            createdAt = createdAt,
            updatedAt = updatedAt
          }

instance ToTType' Beam.DiscountTier Domain.Types.DiscountTier.DiscountTier where
  toTType' (Domain.Types.DiscountTier.DiscountTier {..}) = do
    Beam.DiscountTierT
      { Beam.discountId = Kernel.Types.Id.getId discountId,
        Beam.discountValue = discountValue,
        Beam.discountValueType = discountValueType,
        Beam.id = Kernel.Types.Id.getId id,
        Beam.thresholdValue = thresholdValue,
        Beam.tierOrder = tierOrder,
        Beam.createdAt = createdAt,
        Beam.updatedAt = updatedAt
      }
