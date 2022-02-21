{-# OPTIONS_GHC -Wno-orphans #-}

module Domain.Types.TransportStation where

import Beckn.Prelude
import Beckn.Types.Id
import Beckn.Utils.GenericPretty (PrettyShow)

data TransportStation = TransportStation
  { id :: Id TransportStation,
    name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, Show, PrettyShow)

data TransportStationAPIEntity = TransportStationAPIEntity
  { name :: Text,
    stationCode :: Text,
    lat :: Double,
    lon :: Double
  }
  deriving (Generic, ToJSON, FromJSON, ToSchema, PrettyShow, Show)

makeTransportStationAPIEntity :: TransportStation -> TransportStationAPIEntity
makeTransportStationAPIEntity TransportStation {..} =
  TransportStationAPIEntity {..}
