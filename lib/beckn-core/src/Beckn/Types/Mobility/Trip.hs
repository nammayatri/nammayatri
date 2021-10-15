{-# LANGUAGE DuplicateRecordFields #-}

module Beckn.Types.Mobility.Trip where

import Beckn.Types.Core.Price
import Beckn.Types.Core.State as Core
import Beckn.Types.Mobility.Driver
import Beckn.Types.Mobility.Payload
import Beckn.Types.Mobility.Route
import Beckn.Types.Mobility.Stop
import Beckn.Types.Mobility.Vehicle
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Text hiding (drop)
import EulerHS.Prelude hiding (drop, id, state)

data Trip = Trip
  { id :: Text,
    pickup :: Maybe Stop,
    drop :: Maybe Stop,
    state :: Maybe Core.State,
    vehicle :: Maybe Vehicle,
    driver :: Maybe Driver,
    payload :: Payload,
    fare :: Maybe Price,
    route :: Maybe Route,
    -- not in spec
    totalFare :: Maybe Price
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

instance Example Trip where
  example =
    Trip
      { id = idExample,
        pickup = example,
        drop = example,
        state = example,
        vehicle = example,
        driver = example,
        payload = example,
        fare = example,
        route = example,
        totalFare = example
      }
