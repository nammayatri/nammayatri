{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.FerryStation where

import Beckn.Prelude
import Beckn.Types.Id

data FerryStation = FerryStation
  { id :: Id FerryStation,
    name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic)

data FerryStationAPIEntity = FerryStationAPIEntity
  { name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, ToSchema)

makeFerryStationAPIEntity :: FerryStation -> FerryStationAPIEntity
makeFerryStationAPIEntity FerryStation {..} =
  FerryStationAPIEntity {..}
