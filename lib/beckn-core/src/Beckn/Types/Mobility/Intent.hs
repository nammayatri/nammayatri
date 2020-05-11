{-# LANGUAGE DuplicateRecordFields #-}
module Beckn.Types.Mobility.Intent where

import Beckn.Types.Core.Location
import Beckn.Types.Core.Provider
import Beckn.Types.Core.Scalar
import Beckn.Types.Core.ScalarRange
import Beckn.Types.Core.Tag
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Data.Text
import Data.Time
import Data.Generics.Labels
import EulerHS.Prelude

data Intent = Intent
  { domain :: Text,
    origin :: Location,
    destination :: Location,
    time :: LocalTime,
    stops :: [Stop],
    vehicle :: Vehicle,
    providers :: [Provider],
    payload :: Payload,
    transfer_attrs :: Maybe TransferAttrs,
    fare_range :: ScalarRange,
    tags :: [Tag]
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data TransferAttrs = TransferAttrs
  { max_count :: Int,
    max_distance :: Scalar
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Payload = Payload
  { travellers :: TravellerReqInfo,
    luggage :: Luggage
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Luggage = Luggage
  { count :: Int,
    weight_range :: Maybe ScalarRange,
    dimensions :: Maybe Dimension
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data Dimension = Dimension
  { length :: Scalar,
    breadth :: Scalar,
    height :: Scalar
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data TravellerReqInfo = TravellerReqInfo
  { count :: Int
  }
  deriving (Generic, Show, FromJSON, ToJSON)
