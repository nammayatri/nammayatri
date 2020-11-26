{-# LANGUAGE TemplateHaskell #-}

module Beckn.Types.Core.Migration.Location (Location (..)) where

import Beckn.Types.Core.Migration.Address (Address)
import Beckn.Types.Core.Migration.Circle (Circle)
import Beckn.Types.Core.Migration.City (City)
import Beckn.Types.Core.Migration.Country (Country)
import Beckn.Types.Core.Migration.Descriptor (Descriptor)
import Beckn.Types.Core.Migration.GPS (GPS)
import Beckn.Utils.JSON (deriveJSON)
import EulerHS.Prelude

data Location = Location
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _gps :: Maybe GPS,
    _address :: Maybe Address,
    _station_code :: Maybe Text,
    _city :: Maybe City,
    _country :: Maybe Country,
    _circle :: Maybe Circle,
    _polygon :: Maybe Text,
    _3dspace :: Maybe Text
  }
  deriving (Generic, Show)

deriveJSON ''Location 'stripLensPrefixOptions

{- DELETEME: Should it be more like this?:

data Location = Location
  { _id :: Maybe Text,
    _descriptor :: Maybe Descriptor,
    _location :: LocationV
  }

data LocationV
  = Polygon Text
  | Circle Text
  | Space3D Text
  | Gps GPS
  | Address HumanAddress

data HumanAddress = HumanAddress
  { _address :: Maybe Address,
    _station_code :: Maybe Text,
    _city :: Maybe City,
    _country :: Maybe Country,
  }
-}
