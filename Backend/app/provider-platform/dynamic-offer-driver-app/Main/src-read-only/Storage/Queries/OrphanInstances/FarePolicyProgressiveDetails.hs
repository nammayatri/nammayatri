{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyProgressiveDetails where

import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicyProgressiveDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyProgressiveDetails as Beam
import qualified Storage.Queries.Transformers.FarePolicyProgressiveDetails

instance FromTType' Beam.FarePolicyProgressiveDetails Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails where
  fromTType' (Beam.FarePolicyProgressiveDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails
          { baseDistance = baseDistance,
            baseFare = Kernel.Types.Common.mkAmountWithDefault baseFareAmount baseFare,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            deadKmFare = Kernel.Types.Common.mkAmountWithDefault deadKmFareAmount deadKmFare,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            farePolicyId = farePolicyId,
            nightShiftCharge = nightShiftCharge,
            perMinRateDurationBasis = perMinRateDurationBasis,
            pickupCharges = Storage.Queries.Transformers.FarePolicyProgressiveDetails.mkPickupCharges pickupChargesMin pickupChargesMax pickupChargesMinAmount pickupChargesMaxAmount deadKmFare deadKmFareAmount,
            waitingChargeInfo = ((,) <$> waitingCharge <*> freeWatingTime) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicyProgressiveDetails Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails where
  toTType' (Domain.Types.FarePolicyProgressiveDetails.FarePolicyProgressiveDetails {..}) = do
    Beam.FarePolicyProgressiveDetailsT
      { Beam.baseDistance = baseDistance,
        Beam.baseFare = Kernel.Prelude.roundToIntegral baseFare,
        Beam.baseFareAmount = Kernel.Prelude.Just baseFare,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.deadKmFare = Kernel.Prelude.roundToIntegral deadKmFare,
        Beam.deadKmFareAmount = Kernel.Prelude.Just deadKmFare,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.farePolicyId = farePolicyId,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.perMinRateDurationBasis = perMinRateDurationBasis,
        Beam.pickupChargesMax = Kernel.Prelude.Just $ Kernel.Prelude.roundToIntegral ((.pickupChargesMax) pickupCharges),
        Beam.pickupChargesMaxAmount = Kernel.Prelude.Just ((.pickupChargesMax) pickupCharges),
        Beam.pickupChargesMin = Kernel.Prelude.Just $ Kernel.Prelude.roundToIntegral ((.pickupChargesMin) pickupCharges),
        Beam.pickupChargesMinAmount = Kernel.Prelude.Just ((.pickupChargesMin) pickupCharges),
        Beam.freeWatingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
