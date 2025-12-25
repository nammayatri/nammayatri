{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.KioskLocation where

import Data.Aeson
import qualified Domain.Types.Merchant
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data KioskLocation = KioskLocation
  { address :: Kernel.Prelude.Text,
    contact :: Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation,
    landmark :: Kernel.Prelude.Text,
    latitude :: Kernel.Prelude.Double,
    longitude :: Kernel.Prelude.Double,
    merchantId :: Kernel.Types.Id.Id Domain.Types.Merchant.Merchant
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
