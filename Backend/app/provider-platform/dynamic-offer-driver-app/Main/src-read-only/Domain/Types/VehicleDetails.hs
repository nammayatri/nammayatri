{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleDetails where

import Data.Aeson
import qualified Data.Text
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data VehicleDetails = VehicleDetails {acAvailable :: Kernel.Prelude.Bool, id :: Kernel.Types.Id.Id Domain.Types.VehicleDetails.VehicleDetails, make :: Data.Text.Text, model :: Data.Text.Text}
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
