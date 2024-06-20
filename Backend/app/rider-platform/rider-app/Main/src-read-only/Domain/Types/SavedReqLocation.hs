{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE TemplateHaskell #-}
{-# OPTIONS_GHC -Wno-unused-imports #-}

module Domain.Types.SavedReqLocation where

import Data.Aeson
import qualified Domain.Types.Person
import Kernel.Prelude
import qualified Kernel.Types.Id
import qualified Tools.Beam.UtilsTH

data SavedReqLocation = SavedReqLocation
  { id :: Kernel.Types.Id.Id Domain.Types.SavedReqLocation.SavedReqLocation,
    lat :: Kernel.Prelude.Double,
    lon :: Kernel.Prelude.Double,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    door :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    city :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    country :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    building :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    areaCode :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    area :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    createdAt :: Kernel.Prelude.UTCTime,
    updatedAt :: Kernel.Prelude.UTCTime,
    tag :: Kernel.Prelude.Text,
    isMoved :: Kernel.Prelude.Maybe Kernel.Prelude.Bool,
    riderId :: Kernel.Types.Id.Id Domain.Types.Person.Person,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
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
    lon :: Kernel.Prelude.Double,
    placeId :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    state :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    street :: Kernel.Prelude.Maybe Kernel.Prelude.Text,
    tag :: Kernel.Prelude.Text,
    ward :: Kernel.Prelude.Maybe Kernel.Prelude.Text
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)
