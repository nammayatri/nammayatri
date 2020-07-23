{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Trip where

import Beckn.Types.Core.Price
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Route
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Common
import Data.Text
import EulerHS.Prelude

data Trip = Trip
  { id :: Text,
    vehicle :: Maybe Vehicle,
    driver :: Maybe Driver,
    payload :: Payload,
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
        payload = example,
        fare = example,
        route = example
      }
