module Core.OnSearch.Departure where

import Beckn.Prelude

data Departure = Departure
  { id :: Text,
    route_id :: Text,
    start_time :: TimeStamp,
    end_time :: TimeStamp
  }
  deriving (Show, Generic, FromJSON, ToJSON)

newtype TimeStamp = TimeStamp
  { timestamp :: UTCTime
  }
  deriving (Show, Generic, FromJSON, ToJSON)

