{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.BookingLocation where

import Data.Aeson
import qualified Domain.Types.LocationAddress
import qualified Kernel.External.Maps.HasCoordinates
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data BookingLocation = BookingLocation
  { address :: Domain.Types.LocationAddress.LocationAddress,
    id :: Kernel.Types.Id.Id Domain.Types.BookingLocation.BookingLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime
  }
  deriving (Generic, Show, Eq, Kernel.External.Maps.HasCoordinates.HasCoordinates)

data BookingLocationAPIEntity = BookingLocationAPIEntity
  { area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    extras :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    instructions :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    title :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    ward :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, ToJSON, FromJSON, ToSchema)
