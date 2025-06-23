{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.VehicleRouteMapping where

import Data.Aeson
import qualified Data.Time
import Kernel.Prelude
import qualified Tools.Beam.UtilsTH

data VehicleRouteMapping = VehicleRouteMapping
  { createdAt :: Data.Time.UTCTime,
    routeId :: Kernel.Prelude.Text,
    service :: Kernel.Prelude.Text,
    shift :: Kernel.Prelude.Text,
    typeOfService :: Kernel.Prelude.Text,
    updatedAt :: Data.Time.UTCTime,
    vehicleNo :: Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
