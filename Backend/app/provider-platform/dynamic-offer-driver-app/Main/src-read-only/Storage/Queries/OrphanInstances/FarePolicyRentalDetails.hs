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
          { baseFare = Kernel.Prelude.fromMaybe (Kernel.Prelude.fromMaybe 0 baseFare) baseFareAmount,
            currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            deadKmFare = deadKmFare,
            farePolicyId = farePolicyId,
            includedKmPerHr = includedKmPerHr,
            maxAdditionalKmsLimit = maxAdditionalKmsLimit,
            nightShiftCharge = nightShiftCharge,
            perExtraKmRate = Kernel.Prelude.fromMaybe (Kernel.Prelude.fromMaybe 0 perExtraKmRate) perExtraKmRateAmount,
            perExtraMinRate = Kernel.Prelude.fromMaybe (Kernel.Prelude.fromMaybe 0 perExtraMinRate) perExtraMinRateAmount,
            perHourCharge = Kernel.Prelude.fromMaybe (Kernel.Prelude.fromMaybe 0 perHourCharge) perHourChargeAmount,
            plannedPerKmRate = Kernel.Prelude.fromMaybe (Kernel.Prelude.fromMaybe 0 plannedPerKmRate) plannedPerKmRateAmount,
            totalAdditionalKmsLimit = totalAdditionalKmsLimit,
            waitingChargeInfo = ((,) <$> waitingCharge <*> (Kernel.Types.Common.Minutes <$> freeWaitingTime)) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicyRentalDetails Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails where
  toTType' (Domain.Types.FarePolicyRentalDetails.FarePolicyRentalDetails {..}) = do
    Beam.FarePolicyRentalDetailsT
      { Beam.baseFare = Kernel.Prelude.Just baseFare,
        Beam.baseFareAmount = Kernel.Prelude.Just baseFare,
        Beam.currency = Kernel.Prelude.Just currency,
        Beam.deadKmFare = deadKmFare,
        Beam.farePolicyId = farePolicyId,
        Beam.includedKmPerHr = includedKmPerHr,
        Beam.maxAdditionalKmsLimit = maxAdditionalKmsLimit,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.perExtraKmRate = Kernel.Prelude.Just perExtraKmRate,
        Beam.perExtraKmRateAmount = Kernel.Prelude.Just perExtraKmRate,
        Beam.perExtraMinRate = Kernel.Prelude.Just perExtraMinRate,
        Beam.perExtraMinRateAmount = Kernel.Prelude.Just perExtraMinRate,
        Beam.perHourCharge = Kernel.Prelude.Just perHourCharge,
        Beam.perHourChargeAmount = Kernel.Prelude.Just perHourCharge,
        Beam.plannedPerKmRate = Kernel.Prelude.Just plannedPerKmRate,
        Beam.plannedPerKmRateAmount = Kernel.Prelude.Just plannedPerKmRate,
        Beam.totalAdditionalKmsLimit = totalAdditionalKmsLimit,
        Beam.freeWaitingTime = Kernel.Types.Common.getMinutes <$> (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
