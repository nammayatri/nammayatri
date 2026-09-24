{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FarePolicyInterCityDetails where

import qualified Domain.Types.FarePolicy.Common
import qualified Domain.Types.FarePolicyInterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
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
            defaultWaitTimeAtDestination = defaultWaitTimeAtDestination,
            farePolicyId = farePolicyId,
            kmPerPlannedExtraHour = kmPerPlannedExtraHour,
            nightShiftCharge = nightShiftCharge,
            perDayMaxAllowanceInMins = perDayMaxAllowanceInMins,
            perDayMaxHourAllowance = perDayMaxHourAllowance,
            perExtraKmRate = perExtraKmRate,
            perExtraMinRate = perExtraMinRate,
            perHourCharge = perHourCharge,
            perKmRateOneWay = perKmRateOneWay,
            perKmRateRoundTrip = perKmRateRoundTrip,
            stateEntryPermitCharges = stateEntryPermitCharges,
            waitingChargeInfo = ((,) <$> waitingCharge <*> freeWatingTime) <&> (\(wc, ft) -> Domain.Types.FarePolicy.Common.WaitingChargeInfo {waitingCharge = wc, freeWaitingTime = ft})
          }

instance ToTType' Beam.FarePolicyInterCityDetails Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails where
  toTType' (Domain.Types.FarePolicyInterCityDetails.FarePolicyInterCityDetails {..}) = do
    Beam.FarePolicyInterCityDetailsT
      { Beam.baseFare = baseFare,
        Beam.currency = currency,
        Beam.deadKmFare = deadKmFare,
        Beam.defaultWaitTimeAtDestination = defaultWaitTimeAtDestination,
        Beam.farePolicyId = farePolicyId,
        Beam.kmPerPlannedExtraHour = kmPerPlannedExtraHour,
        Beam.nightShiftCharge = nightShiftCharge,
        Beam.perDayMaxAllowanceInMins = perDayMaxAllowanceInMins,
        Beam.perDayMaxHourAllowance = perDayMaxHourAllowance,
        Beam.perExtraKmRate = perExtraKmRate,
        Beam.perExtraMinRate = perExtraMinRate,
        Beam.perHourCharge = perHourCharge,
        Beam.perKmRateOneWay = perKmRateOneWay,
        Beam.perKmRateRoundTrip = perKmRateRoundTrip,
        Beam.stateEntryPermitCharges = stateEntryPermitCharges,
        Beam.freeWatingTime = (.freeWaitingTime) <$> waitingChargeInfo,
        Beam.waitingCharge = (.waitingCharge) <$> waitingChargeInfo
      }
