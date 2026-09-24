{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyProgressiveDetailsPerExtraKmRateSection where

import qualified Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection as Beam

instance FromTType' Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection where
  fromTType' (Beam.FarePolicyProgressiveDetailsPerExtraKmRateSectionT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection
          { baseFareDepreciation = Kernel.Prelude.fromMaybe 0 baseFareDepreciation,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            farePolicyId = farePolicyId,
            perExtraKmRate = perExtraKmRate,
            startDistance = startDistance
          }

instance ToTType' Beam.FarePolicyProgressiveDetailsPerExtraKmRateSection Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection where
  toTType' (Domain.Types.FarePolicyProgressiveDetailsPerExtraKmRateSection.FarePolicyProgressiveDetailsPerExtraKmRateSection {..}) = do
    Beam.FarePolicyProgressiveDetailsPerExtraKmRateSectionT
      { Beam.baseFareDepreciation = Kernel.Prelude.Just baseFareDepreciation,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.farePolicyId = farePolicyId,
        Beam.perExtraKmRate = perExtraKmRate,
        Beam.startDistance = startDistance
      }
