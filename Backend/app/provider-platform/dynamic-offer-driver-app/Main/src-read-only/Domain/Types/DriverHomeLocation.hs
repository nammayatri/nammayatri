{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-dodgy-exports #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.DriverHomeLocation (module Domain.Types.DriverHomeLocation, module ReExport) where

import Data.Aeson
import Domain.Types.Extra.DriverHomeLocation as ReExport
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data DriverHomeLocation = DriverHomeLocation
  { address :: Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    driverId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    id :: Kernel.Types.Id.Id Domain.Types.DriverHomeLocation.DriverHomeLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    tag :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
