{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyDriverExtraFeeBounds where

import qualified Domain.Types.FarePolicyDriverExtraFeeBounds
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyDriverExtraFeeBounds as Beam

instance FromTType' Beam.FarePolicyDriverExtraFeeBounds Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds where
  fromTType' (Beam.FarePolicyDriverExtraFeeBoundsT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds
          { defaultStepFee = Kernel.Types.Common.mkAmountWithDefault defaultStepFeeAmount defaultStepFee,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            farePolicyId = farePolicyId,
            id = id,
            maxFee = Kernel.Types.Common.mkAmountWithDefault maxFeeAmount maxFee,
            minFee = Kernel.Types.Common.mkAmountWithDefault minFeeAmount minFee,
            startDistance = startDistance,
            stepFee = Kernel.Types.Common.mkAmountWithDefault stepFeeAmount stepFee
          }

instance ToTType' Beam.FarePolicyDriverExtraFeeBounds Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds where
  toTType' (Domain.Types.FarePolicyDriverExtraFeeBounds.FarePolicyDriverExtraFeeBounds {..}) = do
    Beam.FarePolicyDriverExtraFeeBoundsT
      { Beam.defaultStepFee = Kernel.Prelude.roundToIntegral defaultStepFee,
        Beam.defaultStepFeeAmount = Kernel.Prelude.Just defaultStepFee,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.farePolicyId = farePolicyId,
        Beam.id = id,
        Beam.maxFee = Kernel.Prelude.roundToIntegral maxFee,
        Beam.maxFeeAmount = Kernel.Prelude.Just maxFee,
        Beam.minFee = Kernel.Prelude.roundToIntegral minFee,
        Beam.minFeeAmount = Kernel.Prelude.Just minFee,
        Beam.startDistance = startDistance,
        Beam.stepFee = Kernel.Prelude.roundToIntegral stepFee,
        Beam.stepFeeAmount = Kernel.Prelude.Just stepFee
      }
