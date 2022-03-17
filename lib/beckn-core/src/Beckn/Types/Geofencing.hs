module Beckn.Types.Geofencing where

import Beckn.Utils.Dhall
import EulerHS.Prelude

data GeoRestriction
  = Unrestricted
  | Regions [Text]
  deriving (Show, Generic, FromDhall)

data GeofencingConfig = GeofencingConfig
  { origin :: GeoRestriction,
    destination :: GeoRestriction
  }
  deriving (Show, Generic, FromDhall)
