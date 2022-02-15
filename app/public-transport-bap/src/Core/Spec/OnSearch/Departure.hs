module Core.Spec.OnSearch.Departure where

import Beckn.Prelude

data Departure = Departure
  { id :: Text,
    route_id :: Text,
    start_time :: StartTime,
    end_time :: EndTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype StartTime = StartTime
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)

newtype EndTime = EndTime
  { timestamp :: UTCTime
  }
  deriving (Generic, Show, FromJSON, ToJSON, ToSchema)
