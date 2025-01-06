{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.KioskLocationTranslation where

import Data.Aeson
import qualified Domain.Types.KioskLocation
import qualified Kernel.External.Types
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data KioskLocationTranslation = KioskLocationTranslation
  { address :: Kernel.Prelude.Text,
    kioskLocationId :: Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation,
    landmark :: Kernel.Prelude.Text,
    language :: Kernel.External.Types.Language
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
