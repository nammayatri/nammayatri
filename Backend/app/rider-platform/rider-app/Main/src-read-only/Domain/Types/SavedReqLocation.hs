{-# LANGUAGE ApplicativeDo #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SavedReqLocation where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SavedReqLocation = SavedReqLocation
  { area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    id :: Kernel.Types.Id.Id Domain.Types.SavedReqLocation.SavedReqLocation,
    isMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    lat :: Kernel.Prelude.Double,
    locationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lon :: Kernel.Prelude.Double,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tag :: Kernel.Prelude.Text,
    updatedAt :: Kernel.Prelude.UTCTime,
    ward :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data SavedReqLocationAPIEntity = SavedReqLocationAPIEntity
  { area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lat :: Kernel.Prelude.Double,
    locationName :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    lon :: Kernel.Prelude.Double,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tag :: Kernel.Prelude.Text,
    ward :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
