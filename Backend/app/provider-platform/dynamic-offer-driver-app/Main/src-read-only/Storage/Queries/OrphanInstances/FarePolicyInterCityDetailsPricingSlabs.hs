{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyInterCityDetailsPricingSlabs where

import qualified Domain.Types.FarePolicyInterCityDetailsPricingSlabs
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyInterCityDetailsPricingSlabs as Beam

instance FromTType' Beam.FarePolicyInterCityDetailsPricingSlabs Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs where
  fromTType' (Beam.FarePolicyInterCityDetailsPricingSlabsT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs
          { distancePercentage = distancePercentage,
            farePercentage = farePercentage,
            farePolicyId = farePolicyId,
            includeActualDistPercentage = includeActualDistPercentage,
            includeActualTimePercentage = includeActualTimePercentage,
            timePercentage = timePercentage
          }

instance ToTType' Beam.FarePolicyInterCityDetailsPricingSlabs Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs where
  toTType' (Domain.Types.FarePolicyInterCityDetailsPricingSlabs.FarePolicyInterCityDetailsPricingSlabs {..}) = do
    Beam.FarePolicyInterCityDetailsPricingSlabsT
      { Beam.distancePercentage = distancePercentage,
        Beam.farePercentage = farePercentage,
        Beam.farePolicyId = farePolicyId,
        Beam.includeActualDistPercentage = includeActualDistPercentage,
        Beam.includeActualTimePercentage = includeActualTimePercentage,
        Beam.timePercentage = timePercentage
      }
