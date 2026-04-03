{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.KioskLocationTranslation where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Domain.Types.KioskLocation
import qualified Kernel.External.Types
import qualified Tools.Beam.UtilsTH



data KioskLocationTranslation
    = KioskLocationTranslation {address :: Kernel.Prelude.Text,
                                kioskLocationId :: Kernel.Types.Id.Id Domain.Types.KioskLocation.KioskLocation,
                                landmark :: Kernel.Prelude.Text,
                                language :: Kernel.External.Types.Language}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



