{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Trip where

import Beckn.Types.Core.Price
import Beckn.Types.Core.Rating
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Route
import Beckn.Types.Mobility.Tracking
import Beckn.Types.Mobility.Traveller
import Beckn.Types.Mobility.Vehicle
import Data.Generics.Labels
import Data.Text
import Data.Time
import EulerHS.Prelude

data Trip = Trip
  { id :: Text,
    vehicle :: Maybe Vehicle,
    driver :: Maybe TripDriver,
    travellers :: [Traveller],
    tracking :: Tracking,
    corridor_type :: Text, --"FIXED","ON-DEMAND"
    state :: Text, -- schema not available in github, so making it Text
    fare :: Maybe Price,
    route :: Maybe Route
  }
  deriving (Generic, Show, FromJSON, ToJSON)

data TripDriver = TripDriver
  { persona :: Driver,
    rating :: Maybe Rating
  }
  deriving (Generic, Show, FromJSON, ToJSON)
