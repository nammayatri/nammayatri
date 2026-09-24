{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareParametersProgressiveDetails where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FareParametersProgressiveDetails = FareParametersProgressiveDetails
  { currency :: Kernel.Types.Common.Currency,
    deadKmFare :: Kernel.Types.Common.HighPrecMoney,
    extraKmFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    fareParametersId :: Kernel.Prelude.Text,
    rideDurationFare :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
