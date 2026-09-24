{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareParametersRentalDetails where

import qualified Domain.Types.FareParametersRentalDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareParametersRentalDetails as Beam

instance FromTType' Beam.FareParametersRentalDetails Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails where
  fromTType' (Beam.FareParametersRentalDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails
          { currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            deadKmFare = Kernel.Prelude.fromMaybe 0 deadKmFare,
            distBasedFare = Kernel.Types.Common.mkAmountWithDefault distBasedFareAmount distBasedFare,
            distanceUnit = Kernel.Prelude.fromMaybe Kernel.Types.Common.Meter distanceUnit,
            extraDistance = Kernel.Prelude.fromMaybe 0 extraDistance,
            extraDuration = Kernel.Prelude.fromMaybe 0 extraDuration,
            fareParametersId = fareParametersId,
            timeBasedFare = Kernel.Types.Common.mkAmountWithDefault timeBasedFareAmount timeBasedFare
          }

instance ToTType' Beam.FareParametersRentalDetails Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails where
  toTType' (Domain.Types.FareParametersRentalDetails.FareParametersRentalDetails {..}) = do
    Beam.FareParametersRentalDetailsT
      { Beam.currency = Kernel.Prelude.Just currency,
        Beam.deadKmFare = Kernel.Prelude.Just deadKmFare,
        Beam.distBasedFare = Kernel.Prelude.roundToIntegral distBasedFare,
        Beam.distBasedFareAmount = Kernel.Prelude.Just distBasedFare,
        Beam.distanceUnit = Kernel.Prelude.Just distanceUnit,
        Beam.extraDistance = Kernel.Prelude.Just extraDistance,
        Beam.extraDuration = Kernel.Prelude.Just extraDuration,
        Beam.fareParametersId = fareParametersId,
        Beam.timeBasedFare = Kernel.Prelude.roundToIntegral timeBasedFare,
        Beam.timeBasedFareAmount = Kernel.Prelude.Just timeBasedFare
      }
