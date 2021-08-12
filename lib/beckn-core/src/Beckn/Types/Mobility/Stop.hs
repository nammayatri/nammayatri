module Beckn.Types.Mobility.Stop where

import Beckn.Types.Core.Descriptor
import Beckn.Types.Core.Location
import Beckn.Types.Mobility.Transfer
import Beckn.Utils.Example
import Data.OpenApi (ToSchema)
import Data.Time
import EulerHS.Prelude hiding (id)

data Stop = Stop
  { id :: Text,
    descriptor :: Maybe Descriptor,
    location :: Location,
    arrival_time :: StopTime,
    departure_time :: StopTime,
    transfers :: [Transfer]
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

data StopTime = StopTime
  { est :: UTCTime,
    act :: Maybe UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

instance Example StopTime where
  example =
    StopTime
      { est = example,
        act = example
      }

instance Example Stop where
  example =
    Stop
      { id = idExample,
        descriptor = example,
        location = example,
        arrival_time = example,
        departure_time = example,
        transfers = example
      }
