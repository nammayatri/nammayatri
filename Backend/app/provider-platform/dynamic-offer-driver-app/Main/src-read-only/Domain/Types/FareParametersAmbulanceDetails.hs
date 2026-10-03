{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.FareParametersAmbulanceDetails where

import Data.Aeson
import Kernel.Prelude
import qualified Kernel.Types.Common
import qualified Tools.Beam.UtilsTH

data FareParametersAmbulanceDetails = FareParametersAmbulanceDetails
  { cgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    currency :: Kernel.Types.Common.Currency,
    distBasedFare :: Kernel.Types.Common.HighPrecMoney,
    fareParametersId :: Kernel.Prelude.Text,
    platformFee :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney,
    sgst :: Kernel.Prelude.Maybe Kernel.Types.Common.HighPrecMoney
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
