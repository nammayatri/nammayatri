{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyRentalDetails where

import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicyRentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyRentalDetails as Beam

instance FromTType' Beam.FarePolicyRentalDetails Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails where
  fromTType' (Beam.FarePolicyRentalDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails
          { baseFare = Kernel.Types.Common.mkAmountWithDefault baseFareAmount baseFare,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            deadKmFare = deadKmFare,
            farePolicyId = farePolicyId,
            includedKmPerHr = includedKmPerHr,
            maxAdditionalKmsLimit = maxAdditionalKmsLimit,
            nightShiftCharge = nightShiftCharge,
            perExtraKmRate = Kernel.Types.Common.mkAmountWithDefault perExtraKmRateAmount perExtraKmRate,
            perExtraMinRate = Kernel.Types.Common.mkAmountWithDefault perExtraMinRateAmount perExtraMinRate,
            perHourCharge = Kernel.Types.Common.mkAmountWithDefault perHourChargeAmount perHourCharge,
            plannedPerKmRate = Kernel.Types.Common.mkAmountWithDefault plannedPerKmRateAmount plannedPerKmRate,
            totalAdditionalKmsLimit = totalAdditionalKmsLimit,
            waitingChargeInfo = ((,) <$> waitingCharge <*> freeWaitingTime) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicyRentalDetails Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails where
  toTType' (Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails {..}) = do
    Beam.FarePolicyRentalDetailsT
      { Beam.baseFare = Kernel.Prelude.roundToIntegral baseFare,
        Beam.baseFareAmount = Kernel.Prelude.Just baseFare,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.deadKmFare = deadKmFare,
        Beam.farePolicyId = farePolicyId,
        Beam.includedKmPerHr = includedKmPerHr,
        Beam.maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.perExtraKmRate = Kernel.Prelude.roundToIntegral perExtraKmRate,
        Beam.perExtraKmRateAmount = Kernel.Prelude.Just perExtraKmRate,
        Beam.perExtraMinRate = Kernel.Prelude.roundToIntegral perExtraMinRate,
        Beam.perExtraMinRateAmount = Kernel.Prelude.Just perExtraMinRate,
        Beam.perHourCharge = Kernel.Prelude.roundToIntegral perHourCharge,
        Beam.perHourChargeAmount = Kernel.Prelude.Just perHourCharge,
        Beam.plannedPerKmRate = Kernel.Prelude.roundToIntegral plannedPerKmRate,
        Beam.plannedPerKmRateAmount = Kernel.Prelude.Just plannedPerKmRate,
        Beam.totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        Beam.freeWaitingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
