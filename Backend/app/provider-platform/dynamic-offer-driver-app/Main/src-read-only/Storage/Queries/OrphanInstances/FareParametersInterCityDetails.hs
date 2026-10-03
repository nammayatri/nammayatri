{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareParametersInterCityDetails where

import qualified Domain.Types.FareParametersInterCityDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareParametersInterCityDetails as Beam

instance FromTType' Beam.FareParametersInterCityDetails Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails where
  fromTType' (Beam.FareParametersInterCityDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails
          { currency = currency,
            distanceFare = distanceFare,
            extraDistanceFare = extraDistanceFare,
            extraTimeFare = extraTimeFare,
            fareParametersId = fareParametersId,
            pickupCharge = pickupCharge,
            stateEntryPermitCharges = stateEntryPermitCharges,
            timeFare = timeFare
          }

instance ToTType' Beam.FareParametersInterCityDetails Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails where
  toTType' (Domain.Types.FareParametersInterCityDetails.FareParametersInterCityDetails {..}) = do
    Beam.FareParametersInterCityDetailsT
      { Beam.currency = currency,
        Beam.distanceFare = distanceFare,
        Beam.extraDistanceFare = extraDistanceFare,
        Beam.extraTimeFare = extraTimeFare,
        Beam.fareParametersId = fareParametersId,
        Beam.pickupCharge = pickupCharge,
        Beam.stateEntryPermitCharges = stateEntryPermitCharges,
        Beam.timeFare = timeFare
      }
