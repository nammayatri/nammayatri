{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyInterCityDetails where

import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicyInterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FarePolicyInterCityDetails as Beam

instance FromTType' Beam.FarePolicyInterCityDetails Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails where
  fromTType' (Beam.FarePolicyInterCityDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails
          { baseFare = baseFare,
            currency = currency,
            deadKmFare = deadKmFare,
            defaultWaitTimeAtDestination = Kernel.Types.Common.Minutes defaultWaitTimeAtDestination,
            farePolicyId = farePolicyId,
            kmPerPlannedExtraHour = kmPerPlannedExtraHour,
            nightShiftCharge = nightShiftCharge,
            perDayMaxAllowanceInMins = Kernel.Prelude.fmap Kernel.Types.Common.Minutes perDayMaxAllowanceInMins,
            perDayMaxHourAllowance = Kernel.Types.Common.Hours perDayMaxHourAllowance,
            perExtraKmRate = perExtraKmRate,
            perExtraMinRate = perExtraMinRate,
            perHourCharge = perHourCharge,
            perKmRateOneWay = perKmRateOneWay,
            perKmRateRoundTrip = perKmRateRoundTrip,
            stateEntryPermitCharges = stateEntryPermitCharges,
            waitingChargeInfo = ((,) <$> waitingCharge <*> (Kernel.Types.Common.Minutes <$> freeWatingTime)) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicyInterCityDetails Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails where
  toTType' (Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails {..}) = do
    Beam.FarePolicyInterCityDetailsT
      { Beam.baseFare = baseFare,
        Beam.currency = currency,
        Beam.deadKmFare = deadKmFare,
        Beam.defaultWaitTimeAtDestination = Kernel.Types.Common.getMinutes defaultWaitTimeAtDestination,
        Beam.farePolicyId = farePolicyId,
        Beam.kmPerPlannedExtraHour = kmPerPlannedExtraHour,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.perDayMaxAllowanceInMins = Kernel.Prelude.fmap Kernel.Types.Common.getMinutes perDayMaxAllowanceInMins,
        Beam.perDayMaxHourAllowance = Kernel.Types.Common.getHours perDayMaxHourAllowance,
        Beam.perExtraKmRate = perExtraKmRate,
        Beam.perExtraMinRate = perExtraMinRate,
        Beam.perHourCharge = perHourCharge,
        Beam.perKmRateOneWay = perKmRateOneWay,
        Beam.perKmRateRoundTrip = perKmRateRoundTrip,
        Beam.stateEntryPermitCharges = stateEntryPermitCharges,
        Beam.freeWatingTime = Kernel.Types.Common.getMinutes <$> (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
