{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Trip where

import Beckn.Types.Core.Price
import Beckn.Types.Core.Rating
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Route
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Traveller
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Common
import Data.Generics.Labels
import Data.Text
import Data.Time
import EulerHS.Prelude

data Trip = Trip
  { id :: Text,
    vehicle :: Maybe Vehicle,
    driver :: TripDriver,
    travellers :: [Traveller],
    tracking :: Tracking,
    corridor_type :: Text, --"FIXED","ON-DEMAND"
    state :: Text, -- schema not available in github, so making it Text
    fare :: Maybe Price,
    route :: Maybe Route
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example Trip where
  example =
    Trip
      { id = idExample,
        vehicle = example,
        driver = example,
        travellers = example,
        tracking = example,
        corridor_type = "FIXED",
        state = "???",
        fare = example,
        route = example
      }

data TripDriver = TripDriver
  { persona :: Maybe Driver,
    rating :: Maybe Rating
  }
  deriving (Generic, Show, FromJSON, ToJSON)

instance Example TripDriver where
  example =
    TripDriver
      { persona = example,
        rating = example
      }
