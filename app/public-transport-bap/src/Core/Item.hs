module Core.Item where

import Beckn.Prelude
import Core.Price

-- 'Maybe' fields are fields, that is present in request, but we do not use it anyhow
data Item = Item
  { id :: Text,
    category_id :: Maybe Text,
    start_location :: Text,
    end_location :: Text,
    departure_time :: DepartureTime,
    arrival_time :: ArrivalTime,
    price :: Price
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype DepartureTime = DepartureTime
  { timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)

newtype ArrivalTime = ArrivalTime
  { timestamp :: UTCTime
  }
  deriving (Generic, FromJSON, ToJSON, Show, ToSchema)