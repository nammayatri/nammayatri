{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyRentalDetailsPricingSlabs where

import qualified Domain.Types.FarePolicyRentalDetailsPricingSlabs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyRentalDetailsPricingSlabs as Beam

instance FromTType' Beam.FarePolicyRentalDetailsPricingSlabs Domain.Types.FarePolicyRentalDetailsPricingSlabs.FarePolicyRentalDetailsPricingSlabs where
  fromTType' (Beam.FarePolicyRentalDetailsPricingSlabsT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyRentalDetailsPricingSlabs.FarePolicyRentalDetailsPricingSlabs
          { distancePercentage = distancePercentage,
            farePercentage = farePercentage,
            farePolicyId = farePolicyId,
            includeActualDistPercentage = includeActualDistPercentage,
            includeActualTimePercentage = includeActualTimePercentage,
            timePercentage = timePercentage
          }

instance ToTType' Beam.FarePolicyRentalDetailsPricingSlabs Domain.Types.FarePolicyRentalDetailsPricingSlabs.FarePolicyRentalDetailsPricingSlabs where
  toTType' (Domain.Types.FarePolicyRentalDetailsPricingSlabs.FarePolicyRentalDetailsPricingSlabs {..}) = do
    Beam.FarePolicyRentalDetailsPricingSlabsT
      { Beam.distancePercentage = distancePercentage,
        Beam.farePercentage = farePercentage,
        Beam.farePolicyId = farePolicyId,
        Beam.includeActualDistPercentage = includeActualDistPercentage,
        Beam.includeActualTimePercentage = includeActualTimePercentage,
        Beam.timePercentage = timePercentage
      }
