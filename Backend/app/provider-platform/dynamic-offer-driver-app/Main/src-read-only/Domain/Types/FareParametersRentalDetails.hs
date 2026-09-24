{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareParametersRentalDetails where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FareParametersRentalDetails = FareParametersRentalDetails
  { currency :: Kernel.Types.Common.Currency,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    distBasedFare :: Kernel.Types.Common.HighPrecMoney,
    distanceUnit :: Kernel.Types.Common.DistanceUnit,
    extraDistance :: Kernel.Types.Common.Meters,
    extraDuration :: Kernel.Types.Common.Seconds,
    fareParametersId :: Kernel.Prelude.Text,
    timeBasedFare :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
