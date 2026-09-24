{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareParametersInterCityDetails where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FareParametersInterCityDetails = FareParametersInterCityDetails
  { currency :: Kernel.Types.Common.Currency,
    distanceFare :: Kernel.Types.Common.HighPrecMoney,
    extraDistanceFare :: Kernel.Types.Common.HighPrecMoney,
    extraTimeFare :: Kernel.Types.Common.HighPrecMoney,
    fareParametersId :: Kernel.Prelude.Text,
    pickupCharge :: Kernel.Types.Common.HighPrecMoney,
    stateEntryPermitCharges :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    timeFare :: Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
