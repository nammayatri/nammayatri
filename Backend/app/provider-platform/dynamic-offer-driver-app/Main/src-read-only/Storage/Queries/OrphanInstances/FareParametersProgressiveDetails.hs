{-# OPTIONS_GHC -Wno-orphans #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Storage.Queries.OrphanInstances.FareParametersProgressiveDetails where

import qualified Domain.Types.FareParametersProgressiveDetails
import Kernel.Beam.Functions
import Kernel.External.Encryption
import Kernel.Prelude
import qualified Kernel.Prelude
import qualified Kernel.Types.Common
import Kernel.Types.Error
import Kernel.Utils.Common (CacheFlow, EsqDBFlow, MonadFlow, fromMaybeM, getCurrentTime)
import qualified Storage.Beam.FareParametersProgressiveDetails as Beam

instance FromTType' Beam.FareParametersProgressiveDetails Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails where
  fromTType' (Beam.FareParametersProgressiveDetailsT {..}) = do
    pure $
      Just
        Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails
          { currency = Kernel.Prelude.fromMaybe Kernel.Types.Common.INR currency,
            deadKmFare = Kernel.Types.Common.mkAmountWithDefault deadKmFareAmount deadKmFare,
            extraKmFare = Kernel.Types.Common.mkAmountWithDefault extraKmFareAmount <$> extraKmFare,
            fareParametersId = fareParametersId,
            rideDurationFare = rideDurationFare
          }

instance ToTType' Beam.FareParametersProgressiveDetails Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails where
  toTType' (Domain.Types.FareParametersProgressiveDetails.FareParametersProgressiveDetails {..}) = do
    Beam.FareParametersProgressiveDetailsT
      { Beam.currency = Kernel.Prelude.Just currency,
        Beam.deadKmFare = Kernel.Prelude.roundToIntegral deadKmFare,
        Beam.deadKmFareAmount = Kernel.Prelude.Just deadKmFare,
        Beam.extraKmFare = Kernel.Prelude.roundToIntegral <$> extraKmFare,
        Beam.extraKmFareAmount = extraKmFare,
        Beam.fareParametersId = fareParametersId,
        Beam.rideDurationFare = rideDurationFare
      }
