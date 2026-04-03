{-# OPTIONS_GHC -Wno-unused-imports #-}

{-# LANGUAGE ApplicativeDo #-}
module Domain.Types.VehicleDetails where
import Kernel.Prelude
import Data.Aeson
import qualified Kernel.Types.Id
import qualified Data.Text
import qualified Domain.Types.VehicleVariant
import qualified Tools.Beam.UtilsTH



data VehicleDetails
    = VehicleDetails {acAvailable :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
                      capacity :: Kernel.Prelude.Maybe Kernel.Prelude.Int,
                      id :: Kernel.Types.Id.Id Domain.Types.VehicleDetails.VehicleDetails,
                      make :: Data.Text.Text,
                      model :: Data.Text.Text,
                      vehicleVariant :: Domain.Types.VehicleVariant.VehicleVariant,
                      year :: Kernel.Prelude.Maybe Kernel.Prelude.Int}
    deriving (Generic, Show, ToJSON, FromJSON, ToSchema)



